### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to prepare data for various standard plots

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
        mutate(item = factor(item), photo_small = paste("Photos/", photo,"-small.png", sep=""), photo = paste("Photos/", photo,".png", sep=""))
    
    # Add item photo data to main data frame
    plotuse <- plotuse %>% left_join(item_photos, by = "item")
        
    return(plotuse)
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
    itemvalues <- masterdata %>% select(Item, Price, Category) %>% rename(item = Item, price = Price, category = Category)
    
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
    itemvalues <- masterdata %>% select(Category, Item, Price) %>% rename(item = Item, price = Price, category = Category)
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
setup_daily_cost_plot <- function(daily_cost, ymax = 40) {
    
    # Set up plot
    p <- ggplot(daily_cost, aes(x = date, y = daily_cost, color = still_active)) +
        geom_point() +
        scale_color_manual(breaks = c(TRUE, FALSE), values=c("mediumseagreen", "indianred1")) +
        geom_line(aes(x = date, y = average_daily_cost), color='steelblue', size=1) +
        scale_y_continuous(limits=c(0,ymax)) + # Set y limit to NA for automatic scale
        labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "Still active")
    
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
        
        # Calculate complete plot dataup until current day
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





################################################################################################
######################################## CATEGORY PLOTS ########################################
################################################################################################

### These functions set up standard category plots with points and images


# Function to setup (multi) category point plot y = cost per use, x = monthly use
setup_category_plot_point <- function(plot_data, categories, xmax, ymax, log_trans=TRUE, avg_lines=TRUE, animate=FALSE) {
    
    # Filter categories
    plot_data <- plot_data %>% filter(category %in% categories)

    # Filter date
    if (animate) {
        plot_data <- plot_data %>% group_by(date) # Group by frame (date)
    } else {
        plot_data <- plot_data %>% filter(date == max(plot_data$date)) # Include only latest date
    }
    
    # Set up plot
    p <- ggplot(
        plot_data, 
        aes(x = use_per_month, y = cost_per_use, colour = category)) +
        geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
        scale_x_continuous(limits=c(NA,xmax)) +
        scale_color_manual(name = "Category", values = category_colors) +
        scale_alpha(range = c(0.5, 1.0)) +
        scale_size(range = c(2, 3)) +
        guides(alpha = FALSE, size = FALSE) +
        labs(x = "Average times used per month", y = "Cost per use (€)")

    if (log_trans) { p <- p + scale_y_continuous(limits=c(NA,ymax), trans="log10") }
    else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }
    
    # Add vertical and horizonal line for variable averages in corresponding category color
    if(avg_lines) {
        for (cat in categories) {
            p <- p + 
                geom_vline(data = plot_data[plot_data$category == cat,], aes(xintercept = avg_use_per_month_divested), size = 0.8, colour = category_colors[cat], linetype = "dotted") +
                geom_hline(data = plot_data[plot_data$category == cat,], aes(yintercept = avg_cost_per_use_divested), size = 0.8, colour = category_colors[cat], linetype = "dotted")
        }
    }
    
    return(p)
}


# Function to setup (multi) category image plot y = cost per use, x = monthly use
setup_category_plot_image <- function(plot_data, categories, xmax, ymax, log_trans=TRUE, animate=FALSE) {

    # Filter data by category
    plot_data <- plot_data %>% filter(category %in% categories)

    # Filter by last date only for non-animated plots
    if (!animate) { plot_data <- plot_data %>% filter(date == max(plot_data$date)) }
    
    # Set up plot
    p <- ggplot(
        plot_data, 
        aes(x = use_per_month, y = cost_per_use)) +
        geom_vline(aes(xintercept = avg_use_per_month_divested), size = 0.8, colour = "darkgray", linetype = "dotted") +
        geom_hline(aes(yintercept = avg_cost_per_use_divested), size = 0.8, colour = "darkgray", linetype = "dotted") +
        geom_image(aes(image = photo), size = 0.08) +
        scale_x_continuous(limits=c(NA,xmax)) +
        labs(x = "Average times used per month", y = "Cost per use (€)")
    
    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax)) }
    else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }
    
    return(p)
}



# Function to setup (multi) category image plot y = cost per use, x = cumulative use
setup_category_cumulative_plot_image <- function(plot_data, categories, xmax, ymax, log_trans=TRUE, trails=FALSE) {
    
    # Filter data by category
    plot_data <- plot_data %>% filter(category %in% categories)
    
    if (!trails) {
        p <- plot_data %>% filter(date == max(plot_data$date)) %>% 
            ggplot(aes(x = cumuse, y = cost_per_use)) +
            geom_image(aes(image = photo), size = 0.08) +
            scale_x_continuous(limits=c(0,xmax)) +
            labs(x = "Cumulative times used", y = "Cost per use (€)")
    } else {
        p <- plot_data %>%
            ggplot(aes(x = cumuse, y = cost_per_use)) +
            geom_point(colour = "lightgray") + 
            geom_image(data = plot_data[plot_data$date == max(plot_data$date),], aes(image = photo), size = 0.08) +
            scale_x_continuous(limits=c(0,xmax)) +
            labs(x = "Cumulative times used", y = "Cost per use (€)")
    }

    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax)) }
    else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }
    
    return(p)
}




