### Threddit.R - Olof Hoverfält - 2018-2021 - hoverfalt.github.io

# Functions to prepare and plot multi-user data

#################################################################################################
###################################### SET UP ENRIVONENT ########################################
#################################################################################################

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
library(googleCloudStorageR)
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

# Set Google Firebase public photo files path
firebase_img_path_items <<- "https://firebasestorage.googleapis.com/v0/b/threddit-297417.appspot.com/o/"
firebase_img_path_plots <<- "https://firebasestorage.googleapis.com/v0/b/threddit-plots/o/"

# Set Firebase Threddit project id
Firebase_project_id <<- "threddit-297417"

# Authenticate  
gcs_auth("threddit-297417-GCS-access-key.json")

# List Firebase buckets
gcs_list_buckets(Firebase_project_id)

# Set default plot bucket name
gcs_global_bucket("threddit-plots") # Plots
#gcs_global_bucket("threddit-297417.appspot.com") # Project default

# Set upload limit to resumable upload to 100MB
gcs_upload_set_limit(upload_limit = 100000000L)



#################################################################################################
######################################### READ DATA #############################################
#################################################################################################

# Read Google Sheets wardrobe and use data
data_file = get_Google_sheet_ID_Z()
raw_data <- read_sheet(data_file, sheet='Use data filtered - Machine readable')
raw_data <- raw_data %>% as.data.frame() %>% filter(!is.na(Category) & !is.na(Item))
colnames(raw_data) <- c("user", "category", "item", "wears", "cpw", "price", "date_purchased", "total_wears")  
raw_data <- raw_data %>% mutate_at(c("wears"), ~replace(., is.na(.), 0))
raw_data$category <- factor(raw_data$category, levels = category_order)

# Save raw_data data.frame to file for easier retrieval (2021-06-14)
save(masterdata,file="Data/Threddit-Z-raw_data.Rda")
# Load data from file
load("Data/Threddit-Z-raw_data.Rda")

# Read Google Sheets user data
data_file = get_Google_sheet_ID_Z2()
user_data <- read_sheet(data_file, sheet='User profiles')
user_data <- user_data %>% as.data.frame()
save(masterdata,file="Data/Threddit-Z-user_data.Rda")
load("Data/Threddit-Z-user_data.Rda")



#################################################################################################
######################################## SET UP PLOTS ###########################################
#################################################################################################


## Average wardrobe size and value by user

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(user) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  as.data.frame()

total_data <- merge(plot_data, user_data, by = c("user"))

p <- total_data %>% setup_user_distribution_plot(xmax = NA, ymax = NA)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_user.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_user.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)




## Average wardrobe size and value by category

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  mutate(average_items = items/length(unique(raw_data$user)), average_value = value / items)

p <- plot_data %>% setup_category_distribution_plot(categories = category_order, xmax = 31, ymax = 80)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_category.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_category.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)

# Get total item value
sum(plot_data$value)




## Item price distribution by category

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jackets and coats"), xmax = 360, ymax = 40, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jackets_and_coats.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Blazers and vests"), xmax = 620, ymax = 30, binwidth = 10, x_break = 25)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Blazers_and_vests.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jumpers and hoodies"), xmax = 190, ymax = 55, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Cardigans and knits"), xmax = 425, ymax = 80, binwidth = 10, x_break = 25)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Cardigans_and_knits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shirts and blouses"), xmax = 260, ymax = 100, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shirts_and_blouses.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("T-shirts and tops"), xmax = 150, ymax = 300, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-T-shirts_and_tops.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 250, ymax = 55, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shorts and skirts"), xmax = 240, ymax = 65, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shorts_and_skirts.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Trousers and jeans"), xmax = 275, ymax = 70, binwidth = 10, x_break = 25)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Trousers_and_jeans.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shoes and footwear"), xmax = 475, ymax = 65, binwidth = 10, x_break = 25)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shoes_and_footwear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Underwear and socks"), xmax = 160, ymax = 400, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Underwear_and_socks.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Nightwear and homewear"), xmax = 130, ymax = 80, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Nightwear_and_homewear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Accessories"), xmax = 625, ymax = 80, binwidth = 10, x_break = 25)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Accessories.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Sportswear"), xmax = 220, ymax = 130, binwidth = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Sportswear.png")




## Number of wardrobe items and share of items worn

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

p <- plot_data %>% setup_share_worn_plot(categories = c("Jackets and coats", "Blazers and vests"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Jackets_and_coats-and-Blazer_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Jackets_and_coats-and-Blazer_and_vests.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Jumpers and hoodies", "Cardigans and knits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Jumpers_and_hoodies-and-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Jumpers_and_hoodies-and-Cardigans_and_knits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shirts and blouses", "T-shirts and tops"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shirts_and_blouses-and-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shirts_and_blouses-and-T-shirts_and_tops.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Dresses and jumpsuits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shorts and skirts", "Trousers and jeans"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shorts_and_skirts-and-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shorts_and_skirts-and-Trousers_and_jeans.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shoes and footwear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shoes_and_footwear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Underwear and socks", "Nightwear and homewear"), xmax = 80)
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Underwear_and_socks-and-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Underwear_and_socks-and-Nightwear_and_homewear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Accessories"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Accessories.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Sportswear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Sportswear.png")




######################################## DEVELOPMENT WIP ###########################################


## Do users wear expensive clothes more often than affordable ones?
## Purchase price and diary wears 

p <- raw_data %>% filter(wears > 0 & price > 0) %>%
  filter(user == "Sophy") %>%
#  filter(category == "Shoes and footwear") %>%
  setup_category_plot(categories = category_order)
p + geom_smooth(method = "lm", se = FALSE)



raw_data



###################################################################################################
######################################## PLOT FUNCTIONS ###########################################
###################################################################################################


## Function: Item price distribution by category
setup_price_distribution_plot <- function(plot_data, categories, xmax = NA, ymax = NA, binwidth = 10, legend = TRUE, x_break = 10) {
  
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
    geom_vline(xintercept = mean(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$price, na.rm=TRUE), y = 0, label=paste("Mean\n", round(mean(plot_data$price, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
    geom_vline(xintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$price, na.rm=TRUE), y = 0, label=paste("Median\n", round(median(plot_data$price, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = x_break)) +
    scale_fill_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    labs(x = "Price", y = "Total number of items by price tier") +
    ggtitle("Item price distribution by gategory (all users)")
 
  if (!legend){ theme(legend.position = "none") }
  
  return(p)
}




## Function: Number of wardrobe items vs share of items worn
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




# Function: to setup category point plot y = purchase price, x = times worn
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




# Function: Average item value and number of items by category (across all users)
setup_category_distribution_plot <- function(plot_data, categories, xmax = NA, ymax = NA) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$average_items) }
  if(is.na(ymax)) { ymax <- max(plot_data$average_value) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = average_items, y = average_value, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_vline(xintercept = mean(plot_data$average_items), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$average_items), y = min(plot_data$average_value), label=round(mean(plot_data$average_items), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = mean(plot_data$average_value), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = mean(plot_data$average_value), label=round(mean(plot_data$average_value), digits = 0)), color =  "darkgrey") +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    geom_text(aes(label=category), vjust=1.5) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = 2)) +
    scale_y_continuous(limits=c(NA,ymax), breaks = seq.int(from = 0, to = ymax, by = 10), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Average number of items", y = "Average item value (purchase price)") +
    ggtitle("Average item value and number of items by category (across all users)")
  
  return(p)
}


# Function: Wardrobe size and value by user (all categories)
setup_user_distribution_plot <- function(plot_data, xmax = NA, ymax = NA) {
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$items) }
  if(is.na(ymax)) { ymax <- max(plot_data$value) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = items, y = value, colour = gender)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_vline(xintercept = mean(plot_data$items), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$items), y = 0, label=round(mean(plot_data$items), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = mean(plot_data$value), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = mean(plot_data$value), label=round(mean(plot_data$value), digits = 0)), color =  "darkgrey") +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = 20)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = 500), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    #scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Number of items", y = "Value of items (at purchase price)") +
    ggtitle("Wardrobe size and value by user (all categories)")
  
  return(p)
}



