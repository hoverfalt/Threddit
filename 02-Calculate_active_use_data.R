### Threddit.R - Olof Hoverfält - 2018-2024 - hoverfalt.github.io

# Functions to calculate active use data
# Input: itemuse (master tidy data)
# Return: cumulativeuse (calculated cumulative use data)


###################################################
############ CALCULATE ACTIVE USE DATA ############
###################################################

# This section includes the core computations of average and cumulative item level use
# from the tidy raw master data

calculate_active_use_data <- function(itemuse){

    # Copy tidy master data set into one for manipulation and store inte global varible
    cumulativeuse <- itemuse

    
    ### Calculate cumulative item use ###
    
    # Retrieve initial cumulative use by item into temporary df
    tempuse_init <- masterdata %>% dplyr::select(Item,'Times used init') %>%
        dplyr::rename(item = Item, used_init = 'Times used init') %>%
        dplyr::mutate(used_init = as.numeric(used_init))
    
    # Merge into cumulativeuse
    cumulativeuse <- merge(cumulativeuse, as.data.frame(tempuse_init), by="item")

    # Calculate final cumulative use based on initial use and cumulative use
    cumulativeuse <- cumulativeuse %>% dplyr::arrange(category, item, date) %>%
        dplyr::mutate(cumuse = ave(used, item, FUN=cumsum))
    cumulativeuse$cumuse <- (cumulativeuse$cumuse + cumulativeuse$used_init)
    
    # Drop temporary intial cumuse per item column
    cumulativeuse$used_init <- NULL


    ### Calculate price per item ###
    
    # Extract item purchase prices into temporary df
    tempprice <- masterdata %>% dplyr::select(Item, Price) %>% dplyr::mutate(Price = as.numeric(Price)) %>%
        dplyr::rename(item = Item, price = Price)
    
    # Define function for calculating cost per use
    calculate_cost_per_use <- function(price, cumuse) { ifelse(cumuse == 0, price, price / cumuse) }
    
    # Calculate cost per use
    cumulativeuse <- merge(cumulativeuse, tempprice, by="item") %>%
        dplyr::mutate(cost_per_use = calculate_cost_per_use(price, cumuse))
    
    # Drop temporary item price column
    cumulativeuse$price <- NULL
    
    
    ### Calculate days active ###
    
    # Define function for calculating days active initially
    calculate_days_active_init <- function(date_start, date_purchased) {
        as.integer(ifelse(date_purchased > date_start, 0, date_start - date_purchased)) }
    
    # Retrieve initial days active by item
    temp_days_active_init <- masterdata %>% dplyr::select(Item,'Date purchased') %>%
        dplyr::rename(item = Item, date_purchased = 'Date purchased') %>%
        dplyr::mutate(days_active_init = calculate_days_active_init(as.Date("2018-01-01", origin = "1899-12-30"), date_purchased))
    
    # Merge into cumulativeuse
    cumulativeuse <- merge(cumulativeuse, temp_days_active_init, by="item")
    cumulativeuse$date_purchased <- NULL
    
    # Define function for calculating total days active count
    calculate_days_active <- function(days_active_init, date_start, date) {
        as.integer(ifelse(date == date_start, days_active_init, 1)) }
    
    # Calculate total days active counts
    cumulativeuse <- cumulativeuse %>%
        dplyr::mutate(days_active = calculate_days_active(days_active_init, as.Date("2018-01-01"), date))
    
    # Calculate final cumulative days active 
    cumulativeuse <- cumulativeuse %>% dplyr::arrange(category, item, date) %>%
        dplyr::mutate(days_active = ave(days_active, item, FUN=cumsum))
    
    # Drop temporary variable
    cumulativeuse$days_active_init <- NULL
    
 
    ### Calculate use per month ###
    
    # Define function for calculating use per month
    calculate_use_per_month <- function(days_active, cumuse) { ifelse(days_active == 0, 0, cumuse / (days_active / 30.5)) }
    
    # Calculate cost per use
    cumulativeuse <- cumulativeuse %>%
        dplyr::mutate(use_per_month = calculate_use_per_month(days_active, cumuse))
    
 
    ### Finalise use data ###
    
    # Order use data by category > item > date
    cumulativeuse <- cumulativeuse %>% arrange(category, item, date)
    
    return(cumulativeuse)
}

#########################################################
############ ADD DIVESTED ITEMS FOR PLOT USE ############
#########################################################

# This section extends the data for divested items based on on the last day active
# in order to show in computations and plots of later dates

calculate_total_use_data <- function(cumulativeuse) {
    
    # Copy data from raw tidy master data 
    totaluse <- cumulativeuse
    
    # Add item status 'active' of all data points to TRUE
    totaluse$active <- TRUE
    
    #length(unique(totaluse$item))
    for (loop_item in unique(totaluse$item)) {
        
        # If item is still active, move to next in loop
        if (as.Date(max(totaluse$date[totaluse$item == loop_item])) >= as.Date(daterange[[length(daterange)]])) { next }
        
        # Extract row of last active and create divested row to replicate
        temp_passive_item <- totaluse %>% filter(item == loop_item) %>%
            filter(date == max(date)) %>%
            mutate(active = FALSE, used = FALSE)
        
        # Calculate number of rows to add to reach end of daterange
        rows_to_add <- as.integer(as.Date(daterange[[length(daterange)]]) - as.Date(temp_passive_item$date))
        
        # Replicate row to fill the daterange
        temp_passive_item <- temp_passive_item %>% slice(rep(1:n(), each = rows_to_add))
        
        ### Turn static dates into incremental to fill the date range
        
        # Create temporary increasing variable to increase dates 
        temp_passive_item$date_increase <- 1
        temp_passive_item <- temp_passive_item %>%
            dplyr::mutate(date_increase = ave(date_increase, FUN=cumsum))
        
        # Increase dates by one each row
        temp_passive_item <- temp_passive_item %>%
            dplyr::mutate(date = date + date_increase)
        
        # Drop temporary variable
        temp_passive_item$date_increase <- NULL
        
        ### Append the item inactive rows to the total 
        totaluse <- rbind(totaluse, temp_passive_item)    
    }
    
    return(totaluse)
}





