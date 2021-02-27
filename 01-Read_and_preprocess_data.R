### Threddit.R - Olof Hoverfält - 2018-2021 - hoverfalt.github.io

# Functions to read and preprocess raw data
# Input: data_file (raw data file name)
# Return: masterdata (cleaned master raw data)
# Sets up daterange (a list of all dates in the raw data set) in the global environment (REFACTOR THIS)

##########################################################
################ READ AND PREPROCESS DATA ################
##########################################################


# Define function to read raw data from Google Sheets, default as set by get_Google_sheet_ID
read_data_GD <- function (data_file = get_Google_sheet_ID()){

    # Read master data from Google Drive
    masterdata <- read_sheet(data_file)
    
    # Set the column number where the data starts (DEPENDENCY)
    date_column_number <- 12
    
    # Remove rows with <NA> in first column
    masterdata <- masterdata[complete.cases(masterdata[ , 1]),]

    # Remove Google Sheet item photo placeholder and details columns
    masterdata <- masterdata %>% select(-'Photo placeholder', -Details)

    # Convert purchase and divestement dates from character to date
    masterdata <- masterdata %>%
        mutate(`Date purchased` = as.Date(`Date purchased`)) %>%
        mutate(`Date divested` = as.Date(`Date divested`))
    
    # Convert item initial times used from character to numeric
    masterdata <- masterdata %>% mutate(`Times used init` = as.numeric(`Times used init`))
    
    # Convert item price from character to double
    masterdata <- masterdata %>% mutate(Price = as.numeric(Price))
    
    # Extract and convert column names from integers to date strings (set global variable)
    daterange <<- lapply(colnames(masterdata[date_column_number:ncol(masterdata)]), function (a) { as.Date(a, format = "%d.%m.%Y") } )
    
    colnames(masterdata)[date_column_number:ncol(masterdata)] <- lapply(daterange, toString)
    
    # Cast daily use variables from 'x' or NA to locigal
#    masterdata[,date_column_number:ncol(masterdata)] <-
#        mutate_all(masterdata[,date_column_number:ncol(masterdata)], function (a) { as.logical(ifelse(is.na(a), FALSE, TRUE))})
    
    return(masterdata)
}

# Define function to read comparison data from Google Sheets
read_data_GD_comparison <- function (data_file = get_Google_sheet_ID()){
    
    # Read comparison data from Google Drive
    comparison_data <- read_sheet(data_file, sheet = "Comparison")
    
    return(comparison_data)
}




#################################################################
################ TRANSFORM DATA INTO TIDY FORMAT ################
#################################################################

# Define function to transform master data into a tidy format
transform_data <- function (masterdata){

    ### Transform masterdata into tidy data with variables: category, item, date, used
    # select: select only relevant columns (Category, Item, and all dates)
    # gather: key = date = column name, value = used = data, Category and Item unchanged
    itemuse <- masterdata %>% select(-Photo, -Price, -'Date purchased', -'Date divested', -'Times used init',
                                     -'Materials', -'Weight', -'Times used', -'Cost per wear') %>%
        gather(key = "date", value = "used", -Category, -Item, na.rm = TRUE) %>%
        rename(category = "Category", item = "Item") %>%
        mutate(date = as.Date(date)) %>%
        arrange(match(category, category_order)) %>%
        as.data.frame()
    
    # Convert category from character to factor
    itemuse$category <- as.factor(itemuse$category)
    
    # Convert item from character to factor
    itemuse$item <- as.factor(itemuse$item)
    
    # Remove data points prior to date purchsed and after date divested
    for (loop_item in unique(itemuse$item)) {
        
        # Set first active use date location, default date_column_number
        date_purchased <- max(masterdata$`Date purchased`[masterdata$Item == loop_item], daterange[[1]], na.rm = TRUE)
        date_divested <- min(masterdata$`Date divested`[masterdata$Item == loop_item], daterange[[length(daterange)]], na.rm = TRUE)
        
        itemuse <- itemuse %>% filter(!((item == loop_item) & (date < date_purchased)))
        itemuse <- itemuse %>% filter(!((item == loop_item) & (date > date_divested)))
    }
    
    # Remove "ADD NEXT" placeholder items (DEPENDENCY)
    itemuse <- itemuse[!(itemuse$item %in% "ADD NEXT"),]
    
    # Order factors by intuitive categories
    itemuse <- itemuse %>% mutate(category=factor(category, levels=category_order))
    
    # Order data by intuitive categories
    itemuse <- itemuse %>% arrange(match(category, category_order))
    
    return(itemuse)
}




# Function to refresh local list of Dropbox share links. This need to be run each time a new item has been added
refresh_share_links <- function (){

    ## Initiate Dropbox API use
    library(rdrop2)
    library(data.table)
    library(httr)
    
    # Auth, needed if local token expired
    dtoken <- drop_auth() 
    dtoken <- drop_auth(new_user = TRUE) 
    ## drop_acc()

    ## Retrieve shared links
    
    # Initiate URL retrieval (BUG: path filtering does not work, instead retreives all links in account)
    raw <- httr::POST(url = "https://api.dropboxapi.com/2/sharing/list_shared_links",
                      #body = list(path="/threddit/ThredditR/Threddit/Photos"),
                      httr::config(token = dtoken),
                      encode = "json") %>% content("parsed")
    
    # Initiate URL list data frame
    item_photo_URLs <- rbindlist(raw$links, fill = TRUE) # Convert JSON to data.frame

    
    # QUICK AND DIRTY
    temp <- data.frame(name = raw$links[[1]]$name, path = raw$links[[1]]$path_lower, photo_url = raw$links[[1]]$url)
    for (i in  2:length(raw$links)) {
        temp <- rbind(temp, data.frame(name = raw$links[[i]]$name, path = raw$links[[i]]$path_lower, photo_url = raw$links[[i]]$url))
    }
    # Add rest of listings using cursor froim initial request (200 retreival limit per API call)
    while(raw$has_more) {
        # Retrieve next lsting with cursor
        raw <- httr::POST(url = "https://api.dropboxapi.com/2/sharing/list_shared_links",
                          body = list(cursor=raw$cursor),
                          httr::config(token = dtoken),
                          encode = "json") %>% content("parsed")
        
        for (i in  1:length(raw$links)) {
            temp <- rbind(temp, data.frame(name = raw$links[[i]]$name, path = raw$links[[i]]$path_lower, photo_url = raw$links[[i]]$url))
        }
    }
    
    item_photo_URLs <- temp
    
    # Filter listing into global variable
    item_photo_URLs <<- item_photo_URLs %>%
        select(item = name, path, photo_url) %>%
        filter(grepl(Threddit_Dropbox_path_identifier, path, fixed = TRUE) == TRUE) %>% # Filter out non-Threddit links using identifier set in environment
        mutate(photo_url = gsub("?dl=0", "?raw=1", photo_url, fixed = TRUE))
    
    
    
    
    
    
    
    
    
    
    # Add rest of listings using cursor froim initial request (200 retreival limit per API call)
    while(raw$has_more) {
        # Retrieve next lsting with cursor
        raw <- httr::POST(url = "https://api.dropboxapi.com/2/sharing/list_shared_links",
                          body = list(cursor=raw$cursor),
                          httr::config(token = dtoken),
                          encode = "json") %>% content("parsed")
        
        item_photo_URLs <- rbind(item_photo_URLs, rbindlist(raw$links, fill = TRUE))
    }
    
    # Filter listing into global variable
    item_photo_URLs <<- item_photo_URLs %>%
        select(-client_modified, -server_modified, -rev, -id, -link_permissions, -preview_type) %>%
        distinct() %>%
        select(item = name, path = path_lower, photo_url = url, size) %>%
        filter(grepl(Threddit_Dropbox_path_identifier, path, fixed = TRUE) == TRUE) %>% # Filter out non-Threddit links using identifier set in environment
        mutate(photo_url = gsub("?dl=0", "?raw=1", photo_url, fixed = TRUE))
    
    # Save temp to disk
    save(item_photo_URLs,file="Data/item_photo_URLs.Rda")

    
    ## Check if there are items for which there is no sharing link
    
    # List files in /Photos folder
    item_photo_listing <- drop_dir(path = "thredditr/Photos", include_media_info = TRUE, include_has_explicit_shared_members = TRUE) %>%
        select(item = name, path = path_display) %>% as.data.frame()
    
    # List items for which there is not a Dropbox share link
    item_link_missing <- item_photo_listing[!(item_photo_listing$item %in% item_photo_URLs$item),]
    
    # Create share links for missing items
    if (length(item_link_missing$path) > 0) {
        sapply(item_link_missing$path, function(path){drop_share(path, requested_visibility = "public")})
        
        # WIP: Add storing results from drop_share into item_photo_URLs
    }
    
}    








### OLD VERSIONS ###


# Define function to read raw data, default "Threddit.xlsx".
read_data <- function (data_file = "Threddit.xlsx"){
    
    # Set the column number where the data starts (DEPENDENCY)
    date_column_number <- 12
    
    #  Read master data from XLSX into masterdata
    masterdata <- read_xlsx(data_file)
    
    # Replace "\r\n" with " " in Item column
    masterdata$Item <- gsub("\r\n", " ", masterdata$Item)
    
    # Remove rows with <NA> in first column
    masterdata <- masterdata[complete.cases(masterdata[ , 1]),]
    
    # Convert purchase and divestement dates from character to date
    masterdata <- masterdata %>%
        mutate(`Date purchased` = as.Date(`Date purchased`)) %>%
        mutate(`Date divested` = as.Date(`Date divested`))
    #    mutate(`Date purchased` = as.Date(as.numeric(`Date purchased`), origin = "1899-12-30")) %>%
    #        mutate(`Date divested` = as.Date(as.numeric(`Date divested`), origin = "1899-12-30")) 
    
    # Convert item initial times used from character to numeric
    masterdata <- masterdata %>% mutate(`Times used init` = as.numeric(`Times used init`))
    
    # Convert item price from character to double
    masterdata <- masterdata %>% mutate(Price = as.numeric(Price))
    
    
    # Extract and convert column names from integers to date strings (set global variable)
    daterange <<-
        lapply(colnames(masterdata[date_column_number:ncol(masterdata)]), function (a) {
            as.Date(as.numeric(a), origin = "1899-12-30") } )
    
    colnames(masterdata)[date_column_number:ncol(masterdata)] <- lapply(daterange, toString)
    
    # Cast daily use variables from 'x' or N/A to locigal
    masterdata[,date_column_number:ncol(masterdata)] <-
        mutate_all(masterdata[,date_column_number:ncol(masterdata)], function (a) { as.logical(ifelse(is.na(a), FALSE, TRUE))})
    
    return(masterdata)
}






