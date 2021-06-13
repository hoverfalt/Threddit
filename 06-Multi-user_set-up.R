
# Load required packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(transformr)
library(roll)
library(googlesheets4)
library(scales)
library(stringr)
library(curl)
library(openssl) # Used in changing MD5 encoding
library(digest) # Used in reading MD5 encoding 
library(httr) # Get GCS file metadata through http request

## Set up static variables

# Set category order (DEPENDENCY)
category_order <<- c("Jackets and coats", "Blazers and vests", "Jumpers and hoodies", "Cardigans and knits", "Shirts and blouses",
                     "T-shirts and tops", "Dresses and jumpsuits", "Shorts and skirts", "Trousers and jeans", "Shoes and footwear",
                     "Underwear and socks", "Nightwear and homewear", "Accessories", "Sportswear", "Other")

# Set gray and white theme
theme_set(theme_gray())

# Set plot palette for 15 categories
category_colors <<- c('#960001', '#fc3334', '#ff9a02', '#ffdb05', '#4cda00', '#34a853', '#00b0f0', '#0070c0',
                      '#002060', '#a860e9', '#7030a0', '#a5a5a5', '#7b7b7b', '#444444', '#000000')

# Set color names by category name for consistent category colors in plots
names(category_colors) <- category_order

# Set guides for daily cost vs cumulative use plots
guides_prices <<- c(5, 10, 20, 50, 100, 200, 400, 800)

# Set logarithmic y scale breaks
plot_log_breaks <<- c(0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 400, 800)

# Set author label to add in the upper right corner of plots
author_label <<- "hoverfalt.github.io"

# Set Dropbox Threddit photo links path identifier used to filter our non-Threddit links 
Threddit_Dropbox_path_identifier <<- "/thredditr/"


# Set Google API Key to use Google Sheets API 
set_Google_API_key()



# Read Google Sheets data
data_file = get_Google_sheet_ID_Z()
raw_data <- read_sheet(data_file, sheet='Use data filtered - Machine readable')
raw_data <- raw_data %>% as.data.frame() %>% filter(!is.na(Category) & !is.na(Item))
colnames(raw_data) <- c("user", "category", "item", "wears", "cpw", "price", "date_purchased", "total_wears")  
raw_data <- raw_data %>% mutate_at(c("wears"), ~replace(., is.na(.), 0))
raw_data$category <- factor(raw_data$category, levels = category_order)


# Save raw_data data.frame to file for easier retrieval (2021-06-13)
save(masterdata,file="Data/Threddit-Z-raw_data.Rda")
# Load data from file
load("Data/Threddit-Z-raw_data.Rda")

category_order
head(raw_data)


## Average wardrobe size and value by category
plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  mutate(average_items = items/length(unique(raw_data$user)), average_value = value / items)

p <- plot_data %>% setup_category_distribution_plot(categories = category_order, xmax = 30, ymax = 80)
p



## Item price distribution by category
p <- raw_data %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 260, ymax = 50, binwidth = 10)
p

  
  

## Amount of items and share of items worn during diary study
plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

p <- plot_data %>% setup_share_worn_plot(categories = c("Jackets and coats", "Blazers and vests"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Jumpers and hoodies", "Cardigans and knits"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shirts and blouses", "T-shirts and tops"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Dresses and jumpsuits"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shorts and skirts", "Trousers and jeans"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shoes and footwear"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Underwear and socks", "Nightwear and homewear"), xmax = 80)
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Accessories"))
p + geom_smooth(method = "lm")

p <- plot_data %>% setup_share_worn_plot(categories = c("Sportswear"))
p + geom_smooth(method = "lm")

category_order





p <- raw_data %>% filter(wears > 0 & price > 0) %>%
  filter(user == "Sophy") %>%
  setup_category_plot(categories = category_order)
p + geom_smooth(method = "lm", se = FALSE)






## Item price distribution by category
setup_price_distribution_plot <- function(plot_data, categories, xmax = NA, ymax = NA, binwidth = 10) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$price) }
#  if(is.na(ymax)) { ymax <- max(plot_data$average_value) }
  author_label_x <- xmax
  author_label_y <- ymax

  p <-ggplot(
    plot_data,
    aes(x = price, fill = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_histogram(binwidth = binwidth) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = 10)) +
    scale_fill_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    labs(x = "Price", y = "Total number of items by price tier") +
    ggtitle("Total number of items by price tier (all users)")
 
  return(p)
}





## Amount of items and share of items worn during diary study
setup_share_worn_plot <- function(plot_data, categories, xmax = NA, ymax = 1) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$items) }
  if(is.na(ymax)) { ymax <- max(plot_data$share_worn) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = items, y = share_worn, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = 2)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = 0.1), labels = scales::percent_format(accuracy = 1L)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    #scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Amount of items in wardrobe", y = "Share of wardrobe items worn at least once during the diary period") +
    ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)")
  
  return(p)
}


# Amount of items and share of items worn
setup_share_worn_plot <- function(plot_data, categories, xmax = NA, ymax = 1) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$items) }
  if(is.na(ymax)) { ymax <- max(plot_data$share_worn) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = items, y = share_worn, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = 2)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = 0.1), labels = scales::percent_format(accuracy = 1L)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    #scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Amount of items in wardrobe", y = "Share of wardrobe items worn at least once during the diary period") +
    ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)")
  
  return(p)
}






# Function to setup category point plot y = purchase price, x = times worn
setup_category_plot <- function(plot_data, categories, xmax = NA, ymax = NA, ybreaks = plot_log_breaks, log_trans=TRUE, avg_lines=TRUE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$wears) }
  if(is.na(ymax)) { ymax <- max(plot_data$price) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
    
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = wears, y = price, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(breaks = seq.int(from = 0, to = xmax, by = 1), limits=c(0,xmax)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Diary wears", y = "Purchase price (€)")
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks) }
  else { p <- p + scale_y_continuous(limits=c(NA,ymax)) }
  
  return(p)
}






