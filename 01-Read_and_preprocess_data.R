### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to read and preprocess raw data
### Input: data_file (raw data file name)
### Return: masterdata (cleaned master raw data)
### Sets up daterange (a list of all dates in the raw data set) in the global environment (REFACTOR THIS)

##########################################################
################ READ AND PREPROCESS DATA ################
##########################################################

# Define function to read raw data, default "Threddit.xlsx".
read_data <- function (data_file = "Threddit.xlsx"){

    #  Read master data from XLSX into masterdata
    masterdata <- read_xlsx(data_file)
    
    # Replace "\r\n" with " " in Item column
    masterdata$Item <- gsub("\r\n", " ", masterdata$Item)
    
    # Remove rows with <NA> in first column
    masterdata <- masterdata[complete.cases(masterdata[ , 1]),]
    
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


#################################################################
################ TRANSFORM DATA INTO TIDY FORMAT ################
#################################################################

# Define function to transform master data into a tidy format
transform_data <- function (masterdata){

    ### Transform masterdata into tidy data with variables: category, item, date, used
    # select: select only relevant columns (Category, Item, and all dates)
    # gather: key = date = column name, value = used = data, Category and Item unchanged
    itemuse <- masterdata %>% select(-Photo, -Price, -'Date purchased', -'Date divested', -'Times used init',
                                     -'Months available', -'Times used', -'Times used per month', -'Cost per time used') %>%
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
        date_purchased <- max(as.Date(masterdata$`Date purchased`[masterdata$Item == loop_item]), as.Date(daterange[[1]]), na.rm = TRUE)
        date_divested <- min(as.Date(masterdata$`Date divested`[masterdata$Item == loop_item]), as.Date(daterange[[length(daterange)]]), na.rm = TRUE)
        
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