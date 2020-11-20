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
    
    # Set up plot (note the negation: !still_active means "All divested")
    p <- ggplot(daily_cost, aes(x = date, y = daily_cost, color = !still_active)) +
        geom_point() +
        scale_color_manual(breaks = c(FALSE, TRUE), values=c("indianred1", "mediumseagreen")) +
        geom_line(aes(x = date, y = average_daily_cost), color='steelblue', size=1) +
        scale_y_continuous(limits=c(0,ymax)) + # Set y limit to NA for automatic scale
        labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "All divested")
    
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

# Function to setup daily cost animation
setup_daily_cost_animation <- function(daily_cost_anim_plot) {

    # Set up animation
    animation <-
        ggplot(daily_cost_anim_plot, aes(x = date, y = daily_cost, color = !still_active)) +
        geom_point(size=1.5) +
        scale_color_manual(breaks = c(FALSE, TRUE), values=c("indianred1", "mediumseagreen")) +
        geom_line(data = na.omit(daily_cost_anim_plot), aes(x = date, y = average_daily_cost), color='steelblue', size=1.5) +
        scale_y_continuous(limits=c(0,40)) + # Set fixed Y (daily cost) limit at 50 to avoid plot scale from jumping around
        labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "All divested") +
        transition_states(day, state_length = 1, transition_length = 0) +
        labs(title = "Date: {closest_state}") + ease_aes('linear')
    
    return(animation)
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

    if (animate) {
        p <- p +
            # Transition_state using date (as opposed to transition_time) avoids multiple dates rendering in the same frame
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
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
        geom_label(aes(x = avg_use_per_month_divested, y = min(cost_per_use), label=round(avg_use_per_month_divested, digits = 1)), color = "darkgray") +
        geom_label(aes(x = min(use_per_month), y = avg_cost_per_use_divested, label=round(avg_cost_per_use_divested, digits = 2)), color = "darkgray") +
        scale_x_continuous(limits=c(NA,xmax)) +
        labs(x = "Average times used per month", y = "Cost per use (€)")
    
    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax)) }
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
setup_category_cumulative_plot_image <- function(plot_data, categories, xmax = NA, ymin = NA, ymax = NA, log_trans=TRUE, trails=FALSE, guides=TRUE) {
    
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
    
    
    # Build plot
    p <- plot_data %>% ggplot(aes(x = cumuse, y = cost_per_use))
    if (trails) { p <- p + geom_point(colour = "lightgray") }
    if (guides){ p <- p + geom_point(data = guides_data, aes(x = cumuse, y = cost_per_use), colour = "lightblue", size = 0.4) }
    p <- p +
        geom_image(data = plot_data[plot_data$date == max(plot_data$date),], aes(image = photo), size = 0.08) +
        scale_x_continuous(limits=c(0,xmax)) +
        labs(x = "Cumulative times used", y = "Cost per use (€)")
    if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(ymin,ymax)) }
    else { p <- p + scale_y_continuous(limits=c(ymin,ymax)) }
    
    return(p)
}




### CATEGORY PLOT - TIMES USED ###

# Function to setup category image plot y = item (listing), x = Times used
setup_category_times_used_plot <- function(plot_data, categories, animate = FALSE) {

    # Initiate times_ised data frame and data
    times_used <- plot_data %>%
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
    
    # Add standard deviation data
    times_used <- merge(times_used, times_used_std, by = "date", all.x = TRUE) 
    
    # Add rownumber variable telling the order in which to plot the items
    times_used <- times_used %>% 
        group_by(date) %>% arrange(active, desc(days_active), item) %>% # Create correct grouping and in-group order
        mutate(rownumber = cumsum(!is.na(item))) %>% # Create counter according to order (!is.na(item) is just something to cumsum that spans the entire date vector)
        ungroup() %>% arrange(date, active, desc(days_active)) %>% # Ungroup and reorder for plotting
        as.data.frame() # Cast tibble to data frame
    
    # Set up animation
    animation <-
        ggplot(times_used, aes(x = rownumber, y = cumuse)) +
        # Annotation for 1 and 2 standard deviation areas (rendered first to be behind the data layer)
        geom_rect(data = times_used[times_used$rownumber == 1,],
                  aes(xmin=0, xmax=max(times_used$rownumber), ymin=std1_low, ymax=std1_high, group = date), alpha=0.15, fill="green") +
        geom_rect(data = times_used[times_used$rownumber == 1,],
                  aes(xmin=0, xmax=max(times_used$rownumber), ymin=std2_low, ymax=std2_high, group = date), alpha=0.15, fill="green") +
        # Bars marking the progress of each item, color coded by wether item is active of divested
        geom_col(data = times_used, aes(x = rownumber, y = cumuse, fill = active, group = date),
                 width = max(times_used$rownumber)/300, position = position_dodge2(preserve = "total", padding = 0)) +
        scale_fill_manual(breaks = c(FALSE, TRUE), values=c("lightgreen", "lightgray")) +
        geom_image(aes(image = photo, group = date), size = 0.08) +
        labs(x = "Item", y = "Times used") +
        coord_flip() + # Flip coordinates to be horizontal (this switches x and y)
        theme(legend.position = "none") + # Remove legend
        # Add numeric labels for standard deviations, 1 and 2, high and low
        geom_label(data = times_used, aes(x = 0, y = std1_low, label=round(std1_low, digits = 0), group = date), color = "darkgray") +
        geom_label(data = times_used, aes(x = 0, y = std1_high, label=round(std1_high, digits = 0), group = date), color = "darkgray") +
        geom_label(data = times_used, aes(x = 0, y = std2_low, label=round(std2_low, digits = 0), group = date), color = "darkgray") +
        geom_label(data = times_used, aes(x = 0, y = std2_high, label=round(std2_high, digits = 0), group = date), color = "darkgray")

    if (animate) {
        animation <- animation +
            # Transition_state using date (as opposed to transition_time) avoids multiple dates rendering in the same frame
            transition_states(date, state_length = 1, transition_length = 0) +
            labs(title = "Date: {closest_state}") + ease_aes('linear')
    }

    return(animation)
}





