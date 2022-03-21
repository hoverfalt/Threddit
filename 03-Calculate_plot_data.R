### Threddit.R - Olof Hoverfält - 2018-2021 - hoverfalt.github.io

# Functions to prepare data for various standard plots


#################################################################################################
################################# SELECT AND ENHANCE PLOT DATA ##################################
#################################################################################################

### This section includes the core computations to prepare the data for standard plots
### Input: totaluse (total use data)
### Return: plotuse (data prepared for plotting)

calculate_plot_data <- function(totaluse){
    
    # Copy data to be plotted from tidy master data
    plotuse <- totaluse
    
    # Order factors by intuitive categories
    plotuse <- plotuse %>% mutate(category=factor(category, levels=category_order))
    
    # Add numeric variable for active/divested (suitable settings for photo: 0.05 & 0.07)
    calculate_plot_size <- function(active) { ifelse(active == TRUE, 0.05, 0.07) }
    plotuse <- plotuse %>% mutate(plot_size = calculate_plot_size(active))
    
    # Extract item photos and convert item to factor matching main data frame
    item_photos <- masterdata %>% distinct(Item, .keep_all = TRUE) %>%
        select(item = Item, photo = Photo) %>%
        mutate(item = factor(item), photo = paste("Photos/", photo,".png", sep=""))

    # Add item photo data to main data frame
    plotuse <- plotuse %>% left_join(item_photos, by = "item")
        
    return(plotuse)
}


#################################################################################################
###################################### CATEGORY DATA TABLES #####################################
#################################################################################################

### Thius function calculates and formats all item data for category data tables for web publishing
### Input: plotuse, masterdata, item_photoURLs
### Output: saves the Threddit-item_listings variable into a file for the CMS to read

calculate_category_data_tables <- function(plotuse, masterdata){

  # Extract and format item listings as a publishing-ready data frame
  
  # Original Dropbox version
  #item_listings <- plotuse %>% filter(date == max(plotuse$date)) %>%
    #select(Category = category, Item = item, 'Times worn' = cumuse, cost_per_use, days_active, use_per_month, active, photo) %>%
    #mutate('Cost per wear' = round(cost_per_use, 2), 'Months active' = round(days_active / 30.5, 0), 'Wears per month' = round(use_per_month, 1)) %>%
    #select(-cost_per_use, -days_active, -use_per_month) %>%
    #merge(masterdata %>% select(Item, Price)) %>%
    #mutate(Status = ifelse(active == TRUE, "Active", "Divested")) %>%
    #merge(item_photo_URLs %>% select(photo = item, Img = photo_url) %>% mutate(photo = paste0("Photos/", photo, sep="")), by = "photo", all.x = TRUE) %>% 
    #mutate(Img = paste0("<img class='item_image' src='", Img ,"'></img>"), collapse="") %>%
    #select(Img, Category, Item, Price, 'Times worn', 'Cost per wear', 'Wears per month', 'Months active', Status, -active, -photo) %>%
    #arrange(Category, Status, desc(`Times worn`))

  # Google Firebase version
  item_listings <- plotuse %>% filter(date == max(plotuse$date)) %>%
    select(Category = category, Item = item, 'Times worn' = cumuse, cost_per_use, days_active, use_per_month, active, photo) %>%
    mutate('Cost per wear' = round(cost_per_use, 2), 'Months active' = round(days_active / 30.5, 0), 'Wears per month' = round(use_per_month, 1)) %>%
    select(-cost_per_use, -days_active, -use_per_month) %>%
    merge(masterdata %>% select(Item, Price)) %>%
    mutate(Status = ifelse(active == TRUE, "Active", "Divested")) %>%
    mutate(photo = str_remove(photo, 'Photos/')) %>%
    mutate(photo = paste0(firebase_img_path_items, photo, "?alt=media", sep="")) %>%
    mutate(Img = paste0("<img class='item_image' src='", photo,"'></img>", sep="")) %>%
    select(Img, Category, Item, Price, 'Times worn', 'Cost per wear', 'Wears per month', 'Months active', Status, -active, -photo) %>%
    arrange(Category, Status, desc(`Times worn`))
  
    
    # Save item listings to a file for access by the Website builder
    save(item_listings,file="Website/Threddit-item_listings.Rda")
    item_listings <<- item_listings

  
  
  # Extract and format category listings as a publishing-ready data frame
  
  # Exclude Sportswear and order
  category_listing <- usetodate_anim %>% filter(date == max(usetodate_anim$date), category != "Sportswear") %>%
    arrange(match(category, category_order))

  # Add categoryvalue (not category_value) describing the active value, not cumulative
  category_listing <- inventory %>% filter(date == max(usetodate_anim$date)) %>%
    arrange(match(category, category_order)) %>%
    select(category, itemcount, categoryvalue) %>%
    merge(category_listing, by = "category")
  
  # Add tag and local url for images (Original Dropbox version)
  #category_listing <- category_listing %>%
    #merge(item_photo_URLs %>% select(photo = item, Img = photo_url) %>% mutate(photo = paste0("Photos/", photo, sep="")), by = "photo", all.x = TRUE) %>% 
    #mutate(Img = paste0("<img class='item_image' src='", Img ,"'></img>"), collapse="")
  
  # Add tag and local url for images (Google Firebase version)
  category_listing <- category_listing %>%
    mutate(photo = str_remove(photo, 'Photos/')) %>%
    mutate(photo = paste0(firebase_img_path_items, photo, "?alt=media", sep="")) %>%
    mutate(Img = paste0("<img class='item_image' src='", photo ,"'></img>"), collapse="")

  # Add row with totals
  category_listing <- category_listing %>%
    summarise(itemcount = sum(itemcount),
              categoryvalue = sum(categoryvalue),
              yearly_cost = sum(yearly_cost)) %>%
    mutate(category = "Total") %>%
    bind_rows(category_listing)
  
  # Calculate and format column names
  category_listing <- category_listing %>%
    mutate(turnover = yearly_cost / categoryvalue) %>%
    select(Img,
           Category = category,
           'Active items' = itemcount,
           'Inventory value' = categoryvalue,
           'Cost per wear' = daily_cost,
           'Days used' = category_use,
           'Yearly cost' = yearly_cost,
           'Inventory turnover' = turnover) %>%
    arrange(match(Category, c("Total", category_order)))
  
  # Save category listings to a file for access by the Website builder
  save(category_listing,file="Website/Threddit-category_listing.Rda")
  category_listing <<- category_listing
}




#################################################################################################
######################################## PORTFOLIO PLOTS ########################################
#################################################################################################


### This function includes the core computations to prepare the data for standard portfolio plots
### Input: plotuse (total use data enhanced for plot use)
### Return: inventory (portfolio level data for portfolio plots)

calculate_portfolio_plot_data <- function(plotuse){

    ### Calculate number and value of items by category
    
    # Count number of active items grouped by category and date
    inventory <- plotuse %>% filter(active == TRUE) %>%
        group_by(category, date) %>%
        summarise(itemcount = n_distinct(item)) %>%
        as.data.frame()
    
    # List item values from master data
    itemvalues <- masterdata %>% select(Item, Price, Category) %>% dplyr::rename(item = Item, price = Price, category = Category)
    
    # Calculate total item inventory value by date and category 
    inventory_value <- plotuse %>%
        filter(active == TRUE) %>%
        merge(itemvalues, all = TRUE) %>%
        group_by(category, date) %>%
        summarise(categoryvalue = sum(price))
    
    # Merge item counts and item values, and order (FALSE to avoid NA category values)
    inventory <- merge(inventory, inventory_value, all = FALSE) %>%
        arrange(match(category, category_order))

    
    ### Calculate 30-day rolling average category use
    
    # Calculate rolling category use as percentage in 30-day window  
    category_use <- plotuse %>%
        group_by(category, date) %>%
        summarise(category_used = ifelse(sum(used) > 0, TRUE, FALSE)) %>%
        mutate(rolling_category_use = roll_sum(category_used, 30)/30)
    
    # Merge inventory and category use data
    inventory <- merge(inventory, category_use, all = FALSE)
    
    ### Calculate monthly inventory turnaround
    category_turnaround <- inventory %>% mutate(category_turnaround = (rolling_category_use * 30) / itemcount)

    # Merge inventory and category use data
    inventory <- merge(inventory, category_turnaround, all = FALSE)
    
    # Remove temporary variables from memory
    rm(category_use)
    rm(category_turnaround)
    
    return(inventory)
}


### This function includes the core computations to prepare the data for portfolio plots
### Input: plotuse (total use data enhanced for plot use)
### Return: inventory (portfolio level data for portfolio plots)

calculate_complete_portfolio_plot_data <- function(plotuse){

    # Count number of items (active and divested) grouped by category and date
    usetodate <- plotuse %>%
        group_by(category, date) %>%
        summarise(cumuse = sum(cumuse))
    
    # List item values from master data
    itemvalues <- masterdata %>% select(Category, Item, Price) %>% dplyr::rename(item = Item, price = Price, category = Category)
    # Remove "ADD NEXT" placeholder items (DEPENDENCY)
    itemvalues <- itemvalues[!(itemvalues$item %in% "ADD NEXT"),]
    
    # Calculate total item value by date and category 
    usetodate_value <- plotuse %>%
        merge(itemvalues, all = FALSE) %>%
        group_by(category, date) %>%
        summarise(category_value = sum(price)) 
    
    # Merge item counts and item values (FALSE to avoid NA values)
    usetodate <- merge(usetodate, usetodate_value, all = FALSE)
    
    # Calculate dailycost
    usetodate <- usetodate %>%
        mutate(daily_cost = category_value / cumuse)
    
    # Calculate category use %
    category_use <- plotuse %>%
        group_by(category, date) %>%
        summarise(category_used = ifelse(sum(used) > 0, TRUE, FALSE)) %>%
        mutate(category_use = cumsum(category_used) / (as.numeric(date - min(date)) + 1)) %>%
        select(-category_used)
    
    # Merge item counts and item values
    usetodate <- merge(usetodate, category_use, all = FALSE)

    # Add yearly cost equivalent
    usetodate <- usetodate %>% mutate(yearly_cost = daily_cost * 365 * category_use)
    
        
    # Remove temporary variables from memory
    rm(category_use)
    rm(usetodate_value)
    
    return(usetodate)
}


# Funtion to set up inventory item count plot
setup_inventory_item_count_plot <- function(inventory){
    
    # Set author label coordinates (upper right corner)
    author_label_x <- max(inventory$date)
    author_label_y <- max(inventory$itemcount)
    
    # Setup plot
    p <- inventory %>% 
        ggplot( aes(x = date, y = itemcount, colour = category)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_line() +
        scale_color_manual(name = "Category", values = category_colors) +
        scale_y_continuous(breaks=seq(0, 100, by = 10)) + # Set y axis break interval
        labs(x = "Date", y = "Active inventory (number of items)")
    
    return(p)
}


# Funtion to set up inventory value by category plot
setup_inventory_value_by_category_plot <- function(inventory){
    
    # Set author label coordinates (upper right corner)
    author_label_x <- max(inventory$date)
    author_label_y <- max(inventory$categoryvalue)
    
    # Setup plot
    p <- inventory %>% 
        ggplot( aes(x = date, y = categoryvalue, colour = category)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_line() +
        scale_color_manual(name = "Category", values = category_colors) +
        scale_y_continuous(breaks=seq(0, 5000, by = 500)) + # Set y axis break interval
        labs(x = "Date", y = "Active inventory value at purchase price (€)")
    
    return(p)
}


# Funtion to set up inventory value stacked by category plot
setup_inventory_value_stacked_plot <- function(inventory, ymax = NA){
    
    # Set author label coordinates (upper right corner)
    author_label_x <- max(inventory$date)
    author_label_y <- inventory %>% group_by(date) %>% summarise(value = sum(categoryvalue)) %>% select(value) %>% max()
    
    # Setup plot
    p <- inventory %>% 
        ggplot( aes(x = date, y = categoryvalue, fill = category)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_area() +
        scale_fill_manual(name = "Category", values = category_colors) +
        scale_y_continuous(limits=c(0,ymax), breaks=seq(0, 15000, by = 1000)) + # Set y axis break interval
        labs(x = "Date", y = "Active inventory value at purchase price (€)")
    
    return(p)
}





### This function calculates daily cost and rolling average for portfolio or selected categories (Sportswear excluded by default but can be overridden)
### Input: plotuse (total use data enhanced for plot use), rolling avg. window, vector of categories to include, vector of categories to exclude
### Return: daily_cost (total cost by day of items used said day)

calculate_daily_cost <- function(plotuse, rolling_average_window = 30, categories_include = category_order, categories_exclude = "Sportswear"){
    
    # Calculate daily cost and n days rolling average (excluding sportswear)
    daily_cost <- plotuse %>%
        filter(!category %in% categories_exclude) %>%
        filter(category %in% categories_include) %>%
        select(item, date, cost_per_use, used, active) %>%
        group_by(item) %>% mutate(daily_cost = min(cost_per_use), still_active = as.logical(prod(active))) %>% # Capture if item is still in use
        filter(used == TRUE) %>% # Exclude dates of items that were not used that date
        group_by(date) %>% summarise(daily_cost = sum(daily_cost), still_active = as.logical(sum(still_active, na.rm = TRUE))) %>%
        arrange(date) %>% mutate(average_daily_cost = roll_mean(daily_cost, rolling_average_window)) %>%
        mutate(average_daily_cost = lead(average_daily_cost, round(rolling_average_window/2), digits = 0)) # Shift rolling avg to midpoint of sample
    
    return(daily_cost)
}

# Function to setup daily cost plot
setup_daily_cost_plot <- function(daily_cost, ymax = 40, ybreaks = 2, seasons = TRUE, legend = TRUE) {

    # Set author label coordinates (upper right corner)
    author_label_x <- max(daily_cost$date)
    author_label_y <- ymax
    
    # Set up plot (note the negation: !still_active means "All divested")
    p <- ggplot(daily_cost, aes(x = date, y = daily_cost, color = !still_active))
    
    # Add season overlay (consider refactoring to avoid hard coded data)
    if (seasons) {
        p <- p +
            annotate("rect", xmin = as.Date("2018-06-22"), xmax = as.Date("2018-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
            annotate("rect", xmin = as.Date("2019-06-22"), xmax = as.Date("2019-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
            annotate("rect", xmin = as.Date("2020-06-22"), xmax = as.Date("2020-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
            annotate("rect", xmin = as.Date("2021-06-22"), xmax = as.Date("2021-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0)
        #            annotate("rect", xmin = as.Date("2020-03-18"), xmax = as.Date("2020-06-21"), ymin = 0, ymax = ymax, alpha = 0.1, fill = "lightpink", size = 0) + # Corona
#            annotate("rect", xmin = as.Date("2020-08-16"), xmax = as.Date("2020-12-21"), ymin = 0, ymax = ymax, alpha = 0.1, fill = "lightpink", size = 0) # Corona
    }

    # Add data, set scales and labels
    p <- p +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_point() +
        scale_color_manual(breaks = c(FALSE, TRUE), values=c("indianred1", "mediumseagreen")) +
        geom_line(aes(x = date, y = average_daily_cost), color='steelblue', size=1) +
#        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        scale_y_continuous(limits=c(0,ymax), breaks=seq(0, 100, by = ybreaks)) + # Set y limit to NA in function call for automatic scale
        labs(x = "Date", y = "Daily cost and 30-day rolling average shifted to midpoint of sample (€)", color = "All divested")
    
    # Hide legend 
    if (!legend){ p <- p + guides(color = FALSE) }
    #    if (!legend){ theme(legend.position = "none") }
    
    return(p)
}


# Same as calculate_daily_cost but for animation (heavy computing)
calculate_daily_cost_anim <- function(plotuse, rolling_average_window = 30, categories_include = category_order, categories_exclude = "Sportswear"){
    
    # Initiatlise empty daily_cost_temp data frame (CHANGE TO A BETTER WAY)
    daily_cost_anim <- plotuse %>%
        select(item, date, cost_per_use, used, active) %>%
        group_by(item) %>% mutate(daily_cost = min(cost_per_use), still_active = as.logical(prod(active))) %>% # Capture if item is still in use
        filter(used == TRUE) %>% # Exclude dates of items that were not used that date
        group_by(date) %>% summarise(daily_cost = sum(daily_cost), still_active = as.logical(sum(still_active, na.rm = TRUE))) %>%
        arrange(date) %>% mutate(average_daily_cost = roll_mean(daily_cost, rolling_average_window, min_obs = 1)) %>%
        mutate(average_daily_cost = lead(average_daily_cost, rolling_average_window/2)) %>% # Shift rolling avg to midpoint of sample
        mutate(day = date) # Add variable for separating day to plot from dates to plot
    daily_cost_anim <- as.data.frame(daily_cost_anim[0,])
    
    # Generate cumulative data by day, including all dates prior to said day (THIS TAKES A WHILE)
    for (i in daterange) {
        
        # Calculate complete plot data up until current day
        daily_cost_temp <- plotuse %>%
            filter(date <= i) %>% # Include data up until loop day only
            filter(!category %in% categories_exclude) %>%
            filter(category %in% categories_include) %>%
            select(item, date, cost_per_use, used, active) %>%
            group_by(item) %>% mutate(daily_cost = min(cost_per_use), still_active = as.logical(prod(active))) %>% # Capture if item is still in use
            filter(used == TRUE) %>% # Exclude dates of items that were not used that date
            group_by(date) %>% summarise(daily_cost = sum(daily_cost), still_active = as.logical(sum(still_active, na.rm = TRUE))) %>%
            arrange(date) %>% mutate(average_daily_cost = roll_mean(daily_cost, rolling_average_window, min_obs = 1)) %>%
            mutate(average_daily_cost = lead(average_daily_cost, round(rolling_average_window/2), digits = 0)) %>% # Shift rolling avg to midpoint of sample
            mutate(day = i) # Add variable for separating day to plot from dates to plot
        
        # Append data for current day to total data
        daily_cost_anim <- rbind(daily_cost_anim, daily_cost_temp)
    }
    
    # Cast daily_cost_anim to data frame
    daily_cost_anim <- as.data.frame(daily_cost_anim)
    
    return(daily_cost_anim)
}


# Function to setup daily cost animation
setup_daily_cost_animation <- function(daily_cost_anim_plot, ymax = 40, seasons = TRUE) {

    # Set author label coordinates (upper right corner)
    author_label_x <- max(daily_cost_anim_plot$date)
    author_label_y <- ymax

    # Set up animation
    animation <- ggplot(daily_cost_anim_plot, aes(x = date, y = daily_cost, color = !still_active))

    # Add season overlay (consider refactoring to avoid hard coded data)
    if (seasons) {
      animation <- animation +
        annotate("rect", xmin = as.Date("2018-06-22"), xmax = as.Date("2018-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
        annotate("rect", xmin = as.Date("2019-06-22"), xmax = as.Date("2019-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
        annotate("rect", xmin = as.Date("2020-06-22"), xmax = as.Date("2020-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0) +
        annotate("rect", xmin = as.Date("2021-06-22"), xmax = as.Date("2021-08-15"), ymin = 0, ymax = ymax, alpha = 0.3, fill = "lightyellow", size = 0)
      #            annotate("rect", xmin = as.Date("2020-03-18"), xmax = as.Date("2020-06-21"), ymin = 0, ymax = ymax, alpha = 0.1, fill = "lightpink", size = 0) + # Corona
      #            annotate("rect", xmin = as.Date("2020-08-16"), xmax = as.Date("2020-12-21"), ymin = 0, ymax = ymax, alpha = 0.1, fill = "lightpink", size = 0) # Corona
    }
    
    animation <- animation + 
      annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
      geom_point(size=1.5) +
      scale_color_manual(breaks = c(FALSE, TRUE), values=c("indianred1", "mediumseagreen")) +
      geom_line(data = na.omit(daily_cost_anim_plot), aes(x = date, y = average_daily_cost), color='steelblue', size=1.5) +
      scale_y_continuous(limits=c(0,ymax)) + # Set fixed Y (daily cost) limit at 40 to avoid plot scale from jumping around
      labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "All divested") +
      transition_states(day, state_length = 1, transition_length = 0) +
      labs(title = "Date: {closest_state}") + ease_aes('linear')
    
    return(animation)
}





# Funtion to set up daily cost vs category use plot
setup_daily_cost_and_category_use_plot <- function(usetodate_anim, ymax = NA, ybreaks = 1, animate = FALSE){
    
    # Animation marker size
    marker_size <- 40
    
    # Use only last date for image plots
    if(!animate) {
        usetodate_anim <- usetodate_anim %>% filter(date == max(usetodate_anim$date))
        marker_size <- 28 # Set still marker size
    }
    
    # Set author label coordinates (upper right corner)
    author_label_x <- 1.0
    if(is.na(ymax)) { author_label_y = max(usetodate_anim$daily_cost) }
    else { author_label_y <- ymax }
    
    # Setup plot
    p <- usetodate_anim %>%
        ggplot(aes(x = category_use, y = daily_cost)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_point(aes(colour = category), size = marker_size) +
        geom_image(aes(image = photo, group = date), size = 0.08) +
        scale_x_continuous(limits=c(0,1), labels = scales::percent_format(accuracy = 1)) +
        scale_y_continuous(limits=c(NA,ymax), breaks=seq(0, 500, by = ybreaks)) +
        scale_color_manual(name = "Category", values = category_colors) +
        scale_size(range = c(1, 10)) +
        guides(size = FALSE) +
        labs(x = "Category frequency of use (share of all days)", y = "Average cost per wear (€)") +
        guides(colour = guide_legend(override.aes = list(size = 2))) # Override plot point size to smaller for legend
    
    if(animate) {
        p <- p +
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
    }
    
    return(p)
}




# Funtion to set up yearly cost vs category use plot
setup_yearly_cost_and_category_use_plot <- function(usetodate_anim, ymax = NA, ybreaks = 200, animate = FALSE){
    
    # Animation marker size
    marker_size <- 40
    
    # Use only last date for image plots
    if(!animate) {
        usetodate_anim <- usetodate_anim %>% filter(date == max(usetodate_anim$date))
        marker_size <- 28 # Set still marker size
    }
    
    # Set author label coordinates (upper right corner)
    author_label_x <- 1.0
    if(is.na(ymax)) { author_label_y = max(usetodate_anim$yearly_cost) }
    else { author_label_y <- ymax }
    
    # Setup plot
    p <- usetodate_anim %>%
        ggplot(aes(x = category_use, y = yearly_cost)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_point(aes(colour = category), size = marker_size) +
        geom_image(aes(image = photo, group = date), size = 0.08) +
        scale_x_continuous(limits=c(0,1), labels = scales::percent_format(accuracy = 1)) +
        scale_y_continuous(limits=c(NA,ymax), breaks=seq(0, 2000, by = ybreaks)) +
        scale_color_manual(name = "Category", values = category_colors) +
        scale_size(range = c(1, 10)) +
        guides(size = FALSE) +
        labs(x = "Category frequency of use (share of all days)", y = "Yearly cost of use (€ / year)") +
        guides(colour = guide_legend(override.aes = list(size = 2))) # Override plot point size to smaller for legend
    
    if(animate) {
        p <- p +
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
    }
    
    return(p)
}










################################################################################################
######################################## CATEGORY PLOTS ########################################
################################################################################################

### The following functions set up category plots with points and images ###

# setup_category_plot_point(plot_data, categories, xmax, ymax, log_trans=TRUE, avg_lines=TRUE, animate=FALSE)
# setup_category_plot_image(plot_data, categories, xmax, ymax, log_trans=TRUE, animate=FALSE)
# setup_category_cumulative_plot_image(plot_data, categories, xmax, ymax, log_trans=TRUE, trails=FALSE)
# setup_category_times_used_plot(plot_data, categories, animate = FALSE)


### CATEGORY PLOT - COST PER USE vs MONTHLY USE ###

# Function to setup (multi) category point plot y = cost per use, x = monthly use
setup_category_plot_point <- function(plot_data, categories, xmax, ymax, ybreaks = plot_log_breaks, log_trans=TRUE, avg_lines=TRUE, animate=FALSE) {
    
    # Filter data by category
    plot_data <- plot_data %>% filter(category %in% categories)
  
    # Filter by last date only for non-animated plots
    if (!animate) { plot_data <- plot_data %>% filter(date == max(plot_data$date)) }

    # Set author label coordinates (upper right corner)
    if(!is.na(xmax)) { author_label_x <- xmax } else { author_label_x <- max(plot_data$use_per_month) }
    if(!is.na(ymax)) { author_label_y <- ymax } else { author_label_y <- max(plot_data$cost_per_use) }
      
    # Set up plot
    p <- ggplot(
        plot_data, 
        aes(x = use_per_month, y = cost_per_use, colour = category)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
        scale_x_continuous(limits=c(NA,xmax)) +
        scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
        scale_alpha(range = c(0.5, 1.0)) +
        scale_size(range = c(2, 3)) +
        guides(alpha = FALSE, size = FALSE) +
        labs(x = "Average times worn per month", y = "Cost per wear (€)")

    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks) }
    else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }
    
    # Add vertical and horizonal line for variable averages in corresponding category color
    if(avg_lines) {
        for (cat in categories) {
            p <- p + 
                geom_vline(data = plot_data[plot_data$category == cat,], aes(xintercept = avg_use_per_month_divested), size = 0.8,
                           colour = category_colors[match(cat, category_order)], linetype = "dotted") +
                geom_hline(data = plot_data[plot_data$category == cat,], aes(yintercept = avg_cost_per_use_divested), size = 0.8,
                           colour = category_colors[match(cat, category_order)], linetype = "dotted") +
                geom_label(data = plot_data[plot_data$category == cat,], aes(x = avg_use_per_month_divested, y = min(cost_per_use), label=round(avg_use_per_month_divested, digits = 1)),
                           color =  category_colors[match(cat, category_order)]) +
                geom_label(data = plot_data[plot_data$category == cat,], aes(x = min(use_per_month), y = avg_cost_per_use_divested, label=paste(round(avg_cost_per_use_divested, digits = 2)," €", sep="")),
                           color  = category_colors[match(cat, category_order)])
        }
    }

    if (animate) {
        p <- p +
            # Transition_state using date (as opposed to transition_time) avoids multiple dates rendering in the same frame
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
    }
    
    return(p)
}


# Function to setup (multi) category image plot y = cost per use, x = monthly use
setup_category_plot_image <- function(plot_data, categories, xmax, ymax, ybreaks = plot_log_breaks, log_trans=TRUE, animate=FALSE, fixed_marker_size=NA) {

    # Filter data by category
    plot_data <- plot_data %>% filter(category %in% categories)

    # Filter by last date only for non-animated plots
    if (!animate) { plot_data <- plot_data %>% filter(date == max(plot_data$date)) }

    # Set author label coordinates (upper right corner)
    if(!is.na(xmax)) { author_label_x <- xmax } else { author_label_x <- max(plot_data$use_per_month) }
    if(!is.na(ymax)) { author_label_y <- ymax } else { author_label_y <- max(plot_data$cost_per_use) }

    # Animation marker size
    marker_size <- 40
    
    # Use only last date for image plots
    if(!animate) {
      usetodate_anim <- usetodate_anim %>% filter(date == max(usetodate_anim$date))
      marker_size <- 28 # Set still marker size
    }
    
    # Override marker size if defined in the funciton call
    if (!is.na(fixed_marker_size)) { marker_size = fixed_marker_size }
            
    # Set up plot
    p <- ggplot(
        plot_data, 
        aes(x = use_per_month, y = cost_per_use)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
        geom_vline(aes(xintercept = avg_use_per_month_divested), size = 0.8, colour = "mediumseagreen", linetype = "dotted") +
        geom_hline(aes(yintercept = avg_cost_per_use_divested), size = 0.8, colour = "mediumseagreen", linetype = "dotted")

    if (nrow(plot_data %>% filter(active == FALSE)) > 0) { # If there are divested items, render them first
      p <- p +
        geom_point(data = plot_data %>% filter(active == FALSE), size = marker_size, color = "mediumseagreen", alpha = 0.6) + # Mark divested items with green circle
        geom_image(data = plot_data %>% filter(active == FALSE), aes(image = photo, group = date), size = 0.08) # Render divested items first
    }
    
    p <- p +
      geom_image(data = plot_data %>% filter(active == TRUE), aes(image = photo, group = date), size = 0.08) + # Render active items on top
      geom_label(aes(x = avg_use_per_month_divested, y = min(cost_per_use), label=round(avg_use_per_month_divested, digits = 1)), color = "mediumseagreen") +
      geom_label(aes(x = min(use_per_month), y = avg_cost_per_use_divested, label=paste(round(avg_cost_per_use_divested, digits = 2)," €", sep="")), color = "mediumseagreen") +
      scale_x_continuous(limits=c(NA,xmax)) +
      labs(x = "Average times worn per month", y = "Cost per wear (€)")
        
    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks) }
    else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }

    if (animate) {
        p <- p +
            # Transition_state using date (as opposed to transition_time) avoids multiple dates rendering in the same frame
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
    }
        
    return(p)
}




### CATEGORY PLOT - COST PER USE vs CUMULATIVE USE ###

# Function to setup (multi) category image plot y = cost per use, x = cumulative use
setup_category_cumulative_plot_image <- function(plot_data, categories, xmax = NA, ymin = NA, ymax = NA, ybreaks = plot_log_breaks, log_trans=TRUE, trails=FALSE, guides=TRUE) {
    
    # Filter data by category
    plot_data <- plot_data %>% filter(category %in% categories)
 
    # Build guides data
    if(guides){
        # Set xmax for guides to data limit if not defined (in the function call (i.e. set automatically by ggplot)
        if (!is.na(xmax)){ guides_length <- xmax }
        else { guides_length = max(plot_data$cumuse)}
        
        # Initiate guides data frame
        guides_data <- data.frame()
        
        # Create guides with 400 observations for each item (purchase price)
        for (i in guides_prices){ # guides_prices is a global variable assumed set
            guides_temp = data.frame(item = rep(paste("Purchase price", i), guides_length))
            guides_temp$date <- max(plot_data$date)
            guides_temp$cumuse <- seq(1, guides_length, 1)
            guides_temp$cost_per_use <- as.numeric(i) / guides_temp$cumuse
            guides_data <- rbind(guides_data, guides_temp)
        }
        
        # Remove guide data with cost_per_use lower than the plot data, to avoid impact on axis limits
        guides_data <- guides_data[guides_data$cost_per_use >= min(plot_data$cost_per_use),]
    }
    
    # Set author label coordinates (upper right corner)
    if(!is.na(xmax)) { author_label_x <- xmax } else { author_label_x <- max(plot_data$cumuse) }
    if(!is.na(ymax)) { author_label_y <- ymax } else { author_label_y <- max(plot_data$cost_per_use) }

    # Set marker size
    marker_size <- 28

    # Build plot
    p <- plot_data %>% ggplot(aes(x = cumuse, y = cost_per_use)) +
        annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1)
      
    if (trails) { p <- p + geom_point(colour = "lightgray") }
    
    if (guides){ p <- p + geom_point(data = guides_data, aes(x = cumuse, y = cost_per_use), colour = "lightblue", size = 0.4) }
    p <- p +
      #geom_point(data = plot_data[plot_data$date == max(plot_data$date) & plot_data$active == FALSE,], size = marker_size, color = "mediumseagreen", alpha = 0.6) + # Mark divested items with green circle
        geom_point(data = plot_data %>% filter(date == max(plot_data$date), active == FALSE), size = marker_size, color = "mediumseagreen", alpha = 0.6) + # Mark divested items with green circle
        geom_image(data = plot_data %>% filter(date == max(plot_data$date), active == FALSE), aes(image = photo), size = 0.08) + # Render divested items first
        geom_image(data = plot_data %>% filter(date == max(plot_data$date), active == TRUE), aes(image = photo), size = 0.08) + # Render active items in top
        #geom_image(data = plot_data[plot_data$date == max(plot_data$date),], aes(image = photo), size = 0.08) + # Render divested items first
        #geom_image(data = plot_data[plot_data$date == max(plot_data$date),], aes(image = photo), size = 0.08) + # Render active items in top
        scale_x_continuous(limits=c(0,xmax)) +
        labs(x = "Times worn", y = "Cost per wear (€)")
    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(ymin,ymax), breaks=ybreaks) }
    else { p <- p + scale_y_continuous(limits=c(ymin,ymax)) }
    
    return(p)
}




### CATEGORY PLOT - TIMES USED ###

# Function to setup category image plot y = item (listing), x = Times used
setup_category_times_used_plot <- function(input_data, categories, animate = FALSE) {

  # Initiate times_ised data frame and data
  times_used <- input_data %>%
    filter(category %in% categories) %>%
    select(item, category, date, cumuse, days_active, active, photo) %>%
    as.data.frame()
  
  # For non-animations, include only data for the last date
  if (!animate) {
    times_used <- times_used %>% filter(date == max(times_used$date))
  }
  
  # Calculate the standard deviations ranges for divested items
  times_used_std <- times_used %>%
    filter(active == FALSE) %>%
    group_by(date) %>%
    summarise(std1_low = quantile(cumuse, 0.32), std1_high = quantile(cumuse, 0.68), std2_low = quantile(cumuse, 0.05), std2_high = quantile(cumuse, 0.95)) %>%
    as.data.frame()
  
  # Add standard deviation data (only if there is any)
  if (nrow(times_used_std) > 0) {
    times_used <- merge(times_used, times_used_std, by = "date", all.x = TRUE)
  } else { # If there is not SD data (no divested item in category), set to NA
    times_used$std1_low <- as.numeric(NA)
    times_used$std1_high <- as.numeric(NA)
    times_used$std2_low <- as.numeric(NA)
    times_used$std2_high <- as.numeric(NA)
  } 
  
  # Add rownumber variable telling the order in which to plot the items
  times_used <- times_used %>% 
    group_by(date) %>% arrange(active, desc(days_active), item) %>% # Create correct grouping and in-group order
    mutate(rownumber = cumsum(!is.na(item))) %>% # Create counter according to order (!is.na(item) is just something to cumsum that spans the entire date vector)
    ungroup() %>% arrange(date, active, desc(days_active)) %>% # Ungroup and reorder for plotting
    as.data.frame() # Cast tibble to data frame
  
  # Set author label coordinates (upper right corner)
  author_label_x <- max(times_used$rownumber)
  author_label_y <- max(times_used$cumuse)
  
  # Animation marker size
  marker_size <- 40
  
  # Use only last date for image plots
  if(!animate) {
    #usetodate_anim <- usetodate_anim %>% filter(date == max(usetodate_anim$date))
    marker_size <- 28 # Set still marker size
  }
  
  
  # Set up animation
  animation <-
    ggplot(times_used, aes(x = rownumber, y = cumuse)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    # Annotation for 1 and 2 standard deviation areas (rendered first to be behind the data layer)
    geom_rect(data = times_used[times_used$rownumber == 1,],
              aes(xmin=0, xmax=max(times_used$rownumber), ymin=std1_low, ymax=std1_high, group = date), alpha=0.15, fill="mediumseagreen") +
    geom_rect(data = times_used[times_used$rownumber == 1,],
              aes(xmin=0, xmax=max(times_used$rownumber), ymin=std2_low, ymax=std2_high, group = date), alpha=0.15, fill="mediumseagreen") +
    # Bars marking the progress of each item, color coded by wether item is active of divested
    geom_col(data = times_used, aes(x = rownumber, y = cumuse, fill = active, group = date),
             width = max(times_used$rownumber)/300, position = position_dodge2(preserve = "total", padding = 0)) +
    scale_fill_manual(breaks = c(FALSE, TRUE), values=c(alpha("mediumseagreen", 0.6), "lightgray"))

  if (nrow(times_used %>% filter(active == FALSE)) > 0) { # If there are divested items, render them first
    animation <- animation +
      geom_point(data = times_used %>% filter(active == FALSE), size = marker_size, color = "mediumseagreen", alpha = 0.6) + # Mark divested items with green circle
      geom_image(data = times_used %>% filter(active == FALSE), aes(image = photo, group = date), size = 0.08) # Render divested items first
  }
      
  animation <- animation +    
    geom_image(data = times_used %>% filter(active == TRUE), aes(image = photo, group = date), size = 0.08) + # Render active items on top
    labs(x = "Items by status (green divested, gray active) and in purchase order (newer higher)", y = "Times worn") +
    coord_flip() + # Flip coordinates to be horizontal (this switches x and y)
    theme(legend.position = "none") + # Remove legend
    # Add numeric labels for standard deviations, 1 and 2, high and low
    geom_label(data = times_used, aes(x = 0, y = std1_low, label=round(std1_low, digits = 0), group = date), color = "mediumseagreen") +
    geom_label(data = times_used, aes(x = 0, y = std1_high, label=round(std1_high, digits = 0), group = date), color = "mediumseagreen") +
    geom_label(data = times_used, aes(x = 0, y = std2_low, label=round(std2_low, digits = 0), group = date), color = "mediumseagreen") +
    geom_label(data = times_used, aes(x = 0, y = std2_high, label=round(std2_high, digits = 0), group = date), color = "mediumseagreen")
  
  if (animate) {
    animation <- animation +
      # Transition_state using date (as opposed to transition_time) avoids multiple dates rendering in the same frame
      transition_states(date, state_length = 1, transition_length = 0) +
      labs(title = "Date: {closest_state}") + ease_aes('linear')
  }
  
  return(animation)
}


### SHOE STEPS USED SPECIAL PLOT ###

# Function to setup shoe steps image plot y = item (listing), x = Steps taken
setup_shoes_steps_plot <- function(input_data) {
  
  # Initiate times_ised data frame and data
  steps_taken <- input_data %>%
    select(item, active, cumsteps, photo) %>%
    as.data.frame()
  
  # Add rownumber variable telling the order in which to plot the items
  steps_taken <- steps_taken %>% 
    ungroup() %>% arrange(active, desc(cumsteps)) %>% # Ungroup and reorder for plotting
    mutate(rownumber = cumsum(!is.na(item))) %>% # Create counter according to order (!is.na(item) is just something to cumsum that spans the entire date vector)
    as.data.frame() # Cast tibble to data frame
  
  # Set author label coordinates (upper right corner)
  author_label_x <- max(steps_taken$rownumber)
  author_label_y <- max(steps_taken$cumsteps)
  
  marker_size <- 28 # Set still marker size
  
  # Set up animation
  p <-
    ggplot(steps_taken, aes(x = rownumber, y = cumsteps)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_col(data = steps_taken, aes(x = rownumber, y = cumsteps, fill = active),
             width = max(steps_taken$rownumber)/300, position = position_dodge2(preserve = "total", padding = 0)) +
    scale_fill_manual(breaks = c(FALSE, TRUE), values=c(alpha("mediumseagreen", 0.6), "lightgray"))
  
  if (nrow(steps_taken %>% filter(active == FALSE)) > 0) { # If there are divested items, render them first
    p <- p +
      geom_point(data = steps_taken %>% filter(active == FALSE), size = marker_size, color = "mediumseagreen", alpha = 0.6) + # Mark divested items with green circle
      geom_image(data = steps_taken %>% filter(active == FALSE), aes(image = photo), size = 0.08) # Render divested items first
  }
  
  p <- p +
    geom_image(data = steps_taken %>% filter(active == TRUE), aes(image = photo), size = 0.08) +
    scale_y_continuous(labels = comma) +
    #scale_x_continuous(breaks=seq(0, max(steps_taken$rownumber), by = 1)) +
    labs(x = "Shoes by status (green divested, gray active)", y = "Total steps") +
    coord_flip() + # Flip coordinates to be horizontal (this switches x and y)
    theme(legend.position = "none") # Remove legend
  
  return(p)
}



