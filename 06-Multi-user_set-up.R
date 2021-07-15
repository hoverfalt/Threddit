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
library(ggrepel) # Enable geom_label_repel


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

# Pre-process and clean data
raw_data <- raw_data %>% as.data.frame() %>% filter(!is.na(category) & !is.na(item))
raw_data <- raw_data %>% mutate_at(c("wears"), ~replace(., is.na(.), 0)) %>% select(-listed_date)

# Turn factor variables into factors
raw_data$user <- factor(raw_data$user, levels = unique(raw_data$user))
raw_data$category <- factor(raw_data$category, levels = category_order)
raw_data$repair_kind <- factor(raw_data$repair_kind, levels = unique(raw_data$repair_kind))
raw_data$reason_not_repaired <- factor(raw_data$reason_not_repaired, levels = unique(raw_data$reason_not_repaired))
raw_data$repair_willing_to_pay <- factor(raw_data$repair_willing_to_pay, levels = unique(raw_data$repair_willing_to_pay))
raw_data$special_care_kind <- factor(raw_data$special_care_kind, levels = unique(raw_data$special_care_kind))

# Save raw_data data.frame to file for easier retrieval (2021-07-09)
save(raw_data, file="Data/Threddit-Z-raw_data.Rda")
# Load data from file
load("Data/Threddit-Z-raw_data.Rda")

# Read Google Sheets user data
data_file = get_Google_sheet_ID_Z2()
user_data <- read_sheet(data_file, sheet='User profiles')
user_data <- user_data %>% as.data.frame()
save(user_data,file="Data/Threddit-Z-user_data.Rda")
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

p <- total_data %>% setup_user_distribution_plot(xmax = 400, ymax = 14000)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_user.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_user.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)

# Count of items
nrow(raw_data)

# Total value of items
sum(raw_data$price, na.rm = TRUE)



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




## Item price distribution by category

# Item price distribution 
p <- raw_data %>% setup_price_distribution_plot(categories = category_order, xmax = 200, ymax = 600, binwidth = 10, x_break = 20, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jackets and coats"), xmax = 360, ymax = 40, binwidth = 10, x_break = 20, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jackets_and_coats.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Blazers and vests"), xmax = 620, ymax = 25, binwidth = 10, x_break = 50, repel_gap = 15)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Blazers_and_vests.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jumpers and hoodies"), xmax = 190, ymax = 55, binwidth = 10, repel_gap = 6)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Cardigans and knits"), xmax = 425, ymax = 80, binwidth = 10, x_break = 25, repel_gap = 12)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Cardigans_and_knits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shirts and blouses"), xmax = 260, ymax = 90, binwidth = 10, x_break = 20, repel_gap = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shirts_and_blouses.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("T-shirts and tops"), xmax = 150, ymax = 300, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-T-shirts_and_tops.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 250, ymax = 55, binwidth = 10, x_break = 20, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shorts and skirts"), xmax = 240, ymax = 65, binwidth = 10, x_break = 20, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shorts_and_skirts.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Trousers and jeans"), xmax = 275, ymax = 70, binwidth = 10, x_break = 20, repel_gap = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Trousers_and_jeans.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shoes and footwear"), xmax = 475, ymax = 60, binwidth = 10, x_break = 50, repel_gap = 16)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shoes_and_footwear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Underwear and socks"), xmax = 160, ymax = 300, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Underwear_and_socks.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Nightwear and homewear"), xmax = 130, ymax = 70, binwidth = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Nightwear_and_homewear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Accessories"), xmax = 625, ymax = 85, binwidth = 10, x_break = 50, repel_gap = 12)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Accessories.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Sportswear"), xmax = 220, ymax = 110, binwidth = 10, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Sportswear.png")




## Number of wardrobe items and share of items worn

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

p <- plot_data %>% setup_share_worn_plot(categories = c("Jackets and coats"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Jackets_and_coats.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Blazers and vests"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Blazer_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Blazer_and_vests.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Jumpers and hoodies"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Cardigans and knits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Cardigans_and_knits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shirts and blouses"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shirts_and_blouses.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("T-shirts and tops"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-T-shirts_and_tops.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Dresses and jumpsuits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shorts and skirts"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shorts_and_skirts.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Trousers and jeans"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Trousers_and_jeans.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shoes and footwear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Shoes_and_footwear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Underwear and socks"), xmax = 80)
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Underwear_and_socks.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Nightwear and homewear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Nightwear_and_homewear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Accessories"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Accessories.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Sportswear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/Z/Z-Wardrobe_items_and_share_worn-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Wardrobe_items_and_share_worn-Sportswear.png")




######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################


## Do users wear expensive clothes more often than affordable ones?
## Purchase price and diary wears 

p <- raw_data %>% filter(wears > 0 & price > 0) %>%
  filter(user == "Sophy") %>%
#  filter(category == "Shoes and footwear") %>%
  setup_category_plot(categories = category_order)
p + geom_smooth(method = "lm", se = FALSE)



# Show one user's dot
p <- total_data %>% mutate(you = ifelse(user == "Kirsten", "You", "Others")) %>%
  setup_user_distribution_plot(xmax = NA, ymax = NA)

# Show question 4
p <- total_data %>% mutate(q4 = `I have a good idea of how many times I have used my clothes before they wear out or are resold or recycled.`) %>%
  setup_user_distribution_plot(xmax = NA, ymax = NA)




str(raw_data)

# List which users repaired 1+ item(s)
raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(user) %>% summarise(repaired_items = n(), repaired_value = sum(price))

# List categories of repaired items
raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(category) %>% summarise(repaired_items = n())


# List number of items by user
raw_data %>% filter(!is.na(item)) %>%
  group_by(user) %>% summarise(total_items = n()) %>%
  as.data.frame()

# List total value of items by user
raw_data %>% filter(!is.na(item)) %>%
  group_by(user) %>% summarise(total_items = sum(price, na.rm = TRUE)) %>%
  as.data.frame()

# List number of items that need repair by user
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  group_by(user) %>% summarise(total_items = n()) %>%
  as.data.frame()

# List value of items that need repair by user
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  group_by(user) %>% summarise(total_items = sum(price, na.rm = TRUE)) %>%
  as.data.frame()

# Reason not repaired
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  as.data.frame() %>%
  group_by(reason_not_repaired) %>%
  summarise(items = n(), value = sum(price))

# List count of items by user for reasons for not repairing  
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  as.data.frame() %>%
  group_by(reason_not_repaired, user) %>%
  tally() %>%
  spread(user, n, fill = 0) %>%
  as.data.frame()

# List count of items by user for reasons for not repairing  
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  as.data.frame() %>%
  group_by(repair_kind, category) %>%
  tally() %>%
  spread(category, n, fill = 0) %>%
  as.data.frame()

# List count of items by user for willingness to pay
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  as.data.frame() %>%
  group_by(user, repair_willing_to_pay) %>%
  tally() %>%
  spread(user, n, fill = 0) %>%
  as.data.frame()

str(raw_data)



### Months available

# Functions for calculating the difference in months between two dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
} 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# Set diary period starting date to use in filtering
diary_starting_date <- as.Date("2021-05-23")

# Calculate months available for all items
raw_data <- raw_data %>% mutate(months_available = mondf(date_purchased, diary_starting_date))

raw_data$months_available[is.na(raw_data$date_purchased)] 

# List wears_per_month as estimated by users
raw_data %>% select(category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  filter(category == "Shoes and footwear") %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / 1.25, digits = 1)) %>%
  arrange(desc(WPM_est)) %>%
  head(20)

# List wears_per_month for specific user and category
raw_data %>% select(user, category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  filter(category == "Shoes and footwear", user == "Quirine") %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / 1.25, digits = 1)) %>%
  arrange(desc(WPM_est)) %>%
  head(20)



# List top wears_per_month during diary period
raw_data %>% select(user, category, item, wears, total_wears, months_available) %>%
  filter(months_available > 0) %>%
  mutate(wears_per_month = wears / 1.25) %>% # 5 weeks diary period = 1.25 months
  arrange(desc(wears_per_month))


## Summarise wears_per_month by user and category

# Set max WPM to every day (avg 30.5 days per month)
WPM_max = 30.5

# Calculate deltas
WPM_delta <-
  raw_data %>% select(user, category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / 1.25, digits = 1)) %>%
  group_by(user, category) %>%
  summarise(est = sum(WPM_est), real = sum(WPM_real)) %>%
  mutate(delta_to_real = round(est/real, digits = 2), delta_to_max = round(est/WPM_max, digits = 2)) %>%
  arrange(desc(delta_to_max)) %>%
  as.data.frame()

# Clear infinite values resulting from zero real uses
WPM_delta$delta_to_real[is.infinite(WPM_delta$delta_to_real)] <- NA


# Show top users or categories
WPM_delta %>% 
  filter(!(category %in% c("Underwear and socks", "Nightwear and homewear", "Accessories", "Sportswear", "Other"))) %>%
  filter(category %in% category_order[8:9]) %>%
  arrange(desc(real)) %>%
  head(15)


# Plot WPM
p <- WPM_delta %>% 
  filter(!(category %in% c("Underwear and socks", "Nightwear and homewear", "Accessories", "Sportswear", "Other"))) %>%
  filter(category %in% category_order[8:9]) %>%
  setup_WPM_delta_plot_categories(xmax = 10, wpm_max = 1.0)
p



###################################################################################################
######################################## PLOT FUNCTIONS ###########################################
###################################################################################################


## Function: Item price distribution by category
setup_price_distribution_plot <- function(plot_data, categories, xmax = NA, ymax = NA, binwidth = 10, legend = TRUE, x_break = 10, repel_gap = 0) {
  
  # Filter data by category
  if (!is.na(categories)) {
    plot_data <- plot_data %>% filter(category %in% categories)
  } 
  
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
    geom_label(aes(x = mean(plot_data$price + repel_gap, na.rm=TRUE), y = 0, label=paste("Mean\n", round(mean(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey", fill = "white") +
    geom_vline(xintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$price - repel_gap, na.rm=TRUE), y = 0, label=paste("Median\n", round(median(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey", fill = "white") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = x_break), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    scale_y_continuous(limits=c(0,ymax)) +
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
    geom_label(aes(x = 0, y = mean(plot_data$average_value), label=paste(round(mean(plot_data$average_value), digits = 0), "€")), color =  "darkgrey") +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    geom_label_repel(aes(label=category)) +
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
    geom_label(aes(x = 0, y = mean(plot_data$value), label = paste(round(mean(plot_data$value), digits = 0), "€")), color =  "darkgrey") +
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


# Function: WPM delta by category
setup_WPM_delta_plot_categories <- function(plot_data,  xmax = NA, ymax = NA, legend = TRUE, overlays = TRUE, wpm_max = 1.0) {
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$delta_to_real, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$delta_to_max, na.rm = TRUE) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = delta_to_real, y = delta_to_max, colour = category))
  
  # Add WPM max overlay
  if (overlays) {
    p <- p +
      annotate("rect", xmin = 1, xmax = xmax, ymin = wpm_max, ymax = ymax, alpha = 0.1, fill = "red", size = 0)
  }
  
  p <- p +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    # 1x lines
    geom_vline(xintercept = 1, color="darkgrey") +
    #geom_label(aes(x = 1, y = ymax, label="1x"), color =  "darkgrey") +
    geom_hline(yintercept = 1, color="darkgrey") +
    #geom_label(aes(x = xmax*0.93, y = 1, label = "1x"), color =  "darkgrey") +
    # Mean lines
    geom_vline(xintercept = mean(plot_data$delta_to_real, na.rm = TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$delta_to_real, na.rm = TRUE), y = ymax, label=paste("Mean", round(mean(plot_data$delta_to_real, na.rm = TRUE), digits = 1))), color =  "darkgrey") +
    geom_hline(yintercept = mean(plot_data$delta_to_max, na.rm = TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = xmax*0.93, y = mean(plot_data$delta_to_max, na.rm = TRUE), label = paste("Mean", round(mean(plot_data$delta_to_max, na.rm = TRUE), digits = 1))), color =  "darkgrey") +
    # Scales
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax+1, by = 1), labels = multiplier_format_x) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = round_any(ymax, 0.5, ceiling), by = 0.5), labels = multiplier_format_y) +
    scale_color_manual(name = "Category", values = category_colors[match(unique(plot_data$category), category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Estimated WPM compared to real during diary period (1x)", y = "Estimated WPM compared to days available (1x)") +
    ggtitle("Wears Per Month (WPM) - User estimate vs max and real")
  
  if (!legend){ theme(legend.position = "none") }
  
  return(p)
}

# Functions for multiplier axis number formatting in plot (e.g. "5x") 

multiplier_format_x <- function (x) {
  number_format(accuracy = 1,
                scale = 1,
                suffix = "x",
                big.mark = ",")(x)
}

multiplier_format_y <- function (x) {
  number_format(accuracy = 0.1,
                scale = 1,
                suffix = "x",
                big.mark = ",")(x)
}

