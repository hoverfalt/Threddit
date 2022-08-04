### Threddit.R - Olof Hoverfält - 2018-2022 - hoverfalt.github.io

# Functions to prepare and plot multi-user data

#################################################################################################
###################################### SET UP ENRIVONENT ########################################
#################################################################################################

# Load required packages
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gplots)
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


# Source required files
source("08-Multi-user-Drive_access.R")


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


# Set plot palette for 3 genders
gender_colors <<- c('#FF6060', '#7578ff', '#57c257')
names(gender_colors) <- c('Female', 'Male', 'Other')


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

# List of users to exclude in full-year use analyses
excluded_users <- get_excluded_users()



#################################################################################################
######################################### READ DATA #############################################
#################################################################################################

# Read Google Sheets wardrobe and use data
data_file = get_Google_sheet_ID_Z()
raw_data <- read_sheet(data_file, sheet='Use data filtered - Machine readable')

# Save Sheet data to avoid refreshing from Google Sheets if computation fails
temp_storage <- raw_data 
# raw_data <- temp_storage
str(raw_data)

# Pre-process and clean data
raw_data <- raw_data %>% as.data.frame() %>% filter(!is.na(category)) %>% filter(!is.na(item))
raw_data <- raw_data %>% mutate_at(c("wears"), ~replace(., is.na(.), 0)) %>% select(-listed_date)

# Turn factor variables into factors
raw_data$user <- factor(raw_data$user, levels = unique(raw_data$user))
raw_data$category <- factor(raw_data$category, levels = category_order)
raw_data$repair_kind <- factor(raw_data$repair_kind, levels = unique(raw_data$repair_kind))
raw_data$reason_not_repaired <- factor(raw_data$reason_not_repaired, levels = unique(raw_data$reason_not_repaired))
raw_data$repair_willing_to_pay <- factor(raw_data$repair_willing_to_pay, levels = unique(raw_data$repair_willing_to_pay))
raw_data$special_care_kind <- factor(raw_data$special_care_kind, levels = unique(raw_data$special_care_kind))

# Save raw_data data.frame to file for easier retrieval (2022-08-03)
save(raw_data, file="Data/Threddit-Z-raw_data.Rda")
# Load data from file
load("Data/Threddit-Z-raw_data.Rda")

# Read Google Sheets user data
data_file = get_Google_sheet_ID_Z2()
user_data <- read_sheet(data_file, sheet='User profiles - MASTER')
user_data <- user_data %>% as.data.frame() %>% filter(!is.na(User))
#names(user_data)[names(user_data) == 'User'] <- 'user'
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

p <- total_data %>% setup_user_distribution_plot(xmax = 400, ymax = 15000)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_user.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_user.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)

# Count of items
nrow(raw_data)

# Total value of items
sum(raw_data$price, na.rm = TRUE)

str(raw_data)
raw_data %>% filter(!is.na(date_divested)) %>% count()
raw_data %>% filter(secondhand) %>% count()
raw_data %>% group_by(user) %>% 
  summarise(items = n(), repaired = n())



## Average wardrobe size and value by category

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  mutate(average_items = items/length(unique(raw_data$user)), average_value = value / items)

p <- plot_data %>% setup_category_distribution_plot(categories = category_order, xmax = 40, ymax = 80)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_category.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_category.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)




## Item price distribution by category

# Item price distribution 
#p <- raw_data %>% setup_price_distribution_plot(categories = category_order, xmax = 200, ymax = 1250, binwidth = 10, x_break = 20, repel_gap = 5)
#ggsave(filename = "Plots/Z/Z-Item_price_distribution.png", p, width = 9, height = 7, dpi = 150, units = "in")
#save_to_cloud_Z("Z-Item_price_distribution.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jackets and coats"), xmax = 360, ymax = 45, binwidth = 10, x_break = 20, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jackets_and_coats.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Blazers and vests"), xmax = 620, ymax = 25, binwidth = 10, x_break = 50, repel_gap = 14)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Blazers_and_vests.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Jumpers and hoodies"), xmax = 190, ymax = 60, binwidth = 10, repel_gap = 6)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Cardigans and knits"), xmax = 425, ymax = 90, binwidth = 10, x_break = 25, repel_gap = 12)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Cardigans_and_knits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shirts and blouses"), xmax = 260, ymax = 100, binwidth = 10, x_break = 20, repel_gap = 10)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shirts_and_blouses.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("T-shirts and tops"), xmax = 150, ymax = 320, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-T-shirts_and_tops.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 250, ymax = 60, binwidth = 10, x_break = 20, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shorts and skirts"), xmax = 240, ymax = 70, binwidth = 10, x_break = 20, repel_gap = 9)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shorts_and_skirts.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Trousers and jeans"), xmax = 275, ymax = 85, binwidth = 10, x_break = 20, repel_gap = 9)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Trousers_and_jeans.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Shoes and footwear"), xmax = 475, ymax = 60, binwidth = 10, x_break = 25, repel_gap = 15)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shoes_and_footwear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Underwear and socks"), xmax = 160, ymax = 350, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Underwear_and_socks.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Nightwear and homewear"), xmax = 130, ymax = 75, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Nightwear_and_homewear.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Accessories"), xmax = 625, ymax = 100, binwidth = 10, x_break = 25, repel_gap = 15)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Accessories.png")

p <- raw_data %>% setup_price_distribution_plot(categories = c("Sportswear"), xmax = 220, ymax = 120, binwidth = 10, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Sportswear.png")


# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


###########################################################################################
## Number of wardrobe items and share of items worn n+ times (include only active items) ##
###########################################################################################


# Calculate plot data where worn is any item with 1+ wears
worn_count <- 1
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears >= worn_count)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

# Plot all categories
for (category in category_order) {
  p <- plot_data %>% setup_share_worn_plot(categories = category)
  filename <- paste("Z-Wardrobe_items_and_share_worn-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

# Calculate plot data where worn is any item with 4+ wears
worn_count <- 4
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears >= worn_count)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

# Plot all categories
for (category in category_order) {
  p <- plot_data %>% setup_share_worn_plot(categories = category, worn_count = worn_count)
  filename <- paste("Z-Wardrobe_items_and_share_worn-n_times-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}





## List number of items by category by user

plot_data <- raw_data %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n()) %>%
  spread(category, items) %>%
  as.data.frame()

write.csv(plot_data,"Plots/Z/Z-Amount_of_active_items_by_categoty_and_user.csv", row.names = FALSE)

str(plot_data)


## List number of unused items by category by user

plot_data <- raw_data %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  filter(worn == 0) %>%
  group_by(user, category) %>%
  summarise(items = n()) %>%
  spread(category, items) %>%
  as.data.frame()

write.csv(plot_data,"Plots/Z/Z-Amount_of_unused_items_by_categoty_and_user.csv", row.names = FALSE)


## List number of items by wears by category

plot_data <- raw_data %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  #filter(worn == 0) %>%
  group_by(category, wears) %>%
  summarise(items = n()) %>%
  spread(category, items) %>%
  as.data.frame()

write.csv(plot_data,"Plots/Z/Z-Amount_of_items_by_wears_by_categoty.csv", row.names = FALSE)

str(plot_data)


#######################################
## Price and Diary wears by category ##
#######################################

plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(category, price, wears)

p <- plot_data %>% setup_category_plot(categories = "Jackets and coats", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 100)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Jackets_and_coats-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Jackets_and_coats-all.png")

p <- plot_data %>% setup_category_plot(categories = "Jackets and coats", log_trans=FALSE, xmax = 60, xbreak=5, ymax = 100)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Jackets_and_coats.png")


p <- plot_data %>% setup_category_plot(categories = "Blazers and vests", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 100)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Blazers_and_vests-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Blazers_and_vests-all.png")

p <- plot_data %>% setup_category_plot(categories = "Blazers and vests", log_trans=FALSE, xmax = 22, xbreak=2, ymax = 200)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Blazers_and_vests.png")


p <- plot_data %>% setup_category_plot(categories = "Jumpers and hoodies", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Jumpers_and_hoodies-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Jumpers_and_hoodies-all.png")

p <- plot_data %>% setup_category_plot(categories = "Jumpers and hoodies", log_trans=FALSE, xmax = 65, xbreak=5, ymax = 100)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Jumpers_and_hoodies.png")


p <- plot_data %>% setup_category_plot(categories = "Cardigans and knits", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Cardigans_and_knits-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Cardigans_and_knits-all.png")

p <- plot_data %>% setup_category_plot(categories = "Cardigans and knits", log_trans=FALSE, xmax = 50, xbreak=5, ymax = 175, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Cardigans_and_knits.png")


p <- plot_data %>% setup_category_plot(categories = "Shirts and blouses", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shirts_and_blouses-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shirts_and_blouses-all.png")

p <- plot_data %>% setup_category_plot(categories = "Shirts and blouses", log_trans=FALSE, xmax = 30, xbreak=5, ymax = 100, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shirts_and_blouses.png")


p <- plot_data %>% setup_category_plot(categories = "T-shirts and tops", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-T-shirts_and_tops-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-T-shirts_and_tops-all.png")

p <- plot_data %>% setup_category_plot(categories = "T-shirts and tops", log_trans=FALSE, xmax = 70, xbreak=5, ymax = 60, ybreak = 5)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-T-shirts_and_tops.png")


p <- plot_data %>% setup_category_plot(categories = "Dresses and jumpsuits", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 50)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Dresses_and_jumpsuits-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Dresses_and_jumpsuits-all.png")

p <- plot_data %>% setup_category_plot(categories = "Dresses and jumpsuits", log_trans=FALSE, xmax = 25, xbreak=5, ymax = 250, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Dresses_and_jumpsuits.png")


p <- plot_data %>% setup_category_plot(categories = "Shorts and skirts", log_trans=FALSE, xmax = NA, xbreak=5, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shorts_and_skirts-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shorts_and_skirts-all.png")

p <- plot_data %>% setup_category_plot(categories = "Shorts and skirts", log_trans=FALSE, xmax = 25, xbreak=5, ymax = 100, ybreak = 10)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shorts_and_skirts.png")


p <- plot_data %>% setup_category_plot(categories = "Trousers and jeans", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 50)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Trousers_and_jeans-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Trousers_and_jeans-all.png")

p <- plot_data %>% setup_category_plot(categories = "Trousers and jeans", log_trans=FALSE, xmax = 80, xbreak=5, ymax = 125, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Trousers_and_jeans.png")


p <- plot_data %>% setup_category_plot(categories = "Shoes and footwear", log_trans=FALSE, xmax = NA, xbreak=20, ymax = NA, ybreak = 50)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shoes_and_footwear-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shoes_and_footwear-all.png")

p <- plot_data %>% setup_category_plot(categories = "Shoes and footwear", log_trans=FALSE, xmax = 100, xbreak=10, ymax = 150, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Shoes_and_footwear.png")


p <- plot_data %>% setup_category_plot(categories = "Underwear and socks", log_trans=FALSE, xmax = NA, xbreak=20, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Underwear_and_socks-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Underwear_and_socks-all.png")

p <- plot_data %>% setup_category_plot(categories = "Underwear and socks", log_trans=FALSE, xmax = 70, xbreak=5, ymax = 50, ybreak = 5)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Underwear_and_socks.png")


p <- plot_data %>% setup_category_plot(categories = "Nightwear and homewear", log_trans=FALSE, xmax = NA, xbreak=20, ymax = NA, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Nightwear_and_homewear-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Nightwear_and_homewear-all.png")

p <- plot_data %>% setup_category_plot(categories = "Nightwear and homewear", log_trans=FALSE, xmax = 100, xbreak=10, ymax = 40, ybreak = 5)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Nightwear_and_homewear.png")


p <- plot_data %>% setup_category_plot(categories = "Accessories", log_trans=FALSE, xmax = NA, xbreak=20, ymax = 1000, ybreak = 100)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Accessories-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Accessories-all.png")

p <- plot_data %>% setup_category_plot(categories = "Accessories", log_trans=FALSE, xmax = 200, xbreak=20, ymax = 200, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Accessories.png")


p <- plot_data %>% setup_category_plot(categories = "Sportswear", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Sportswear-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Sportswear-all.png")

p <- plot_data %>% setup_category_plot(categories = "Sportswear", log_trans=FALSE, xmax = 80, xbreak=5, ymax = 80, ybreak = 5)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Sportswear.png")


p <- plot_data %>% setup_category_plot(categories = "Other", log_trans=FALSE, xmax = NA, xbreak=20, ymax = 420, ybreak = 20)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Other-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Other-all.png")

p <- plot_data %>% setup_category_plot(categories = "Other", log_trans=FALSE, xmax = 100, xbreak=10, ymax = 100, ybreak = 10)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Other.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Other.png")


###############################################
## Plot Diary wears distribution by category ##
###############################################

plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(category, wears)

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jackets and coats", ymax = 110, y_break = 25, xmax = 140, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Jackets_and_coats.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Blazers and vests", ymax = 100, y_break = 25, xmax = 70, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Blazers_and_vests.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jumpers and hoodies", ymax = 110, y_break = 25, xmax = 90, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Cardigans and knits", ymax = 190, y_break = 25, xmax = 100, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Cardigans_and_knits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shirts and blouses", ymax = 275, y_break = 25, xmax = 55, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shirts_and_blouses.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "T-shirts and tops", ymax = 425, y_break = 25, xmax = 100, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-T-shirts_and_tops.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Dresses and jumpsuits", ymax = 250, y_break = 25, xmax = 70, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shorts and skirts", ymax = 160, y_break = 25, xmax = 55, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shorts_and_skirts.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Trousers and jeans", ymax = 200, y_break = 25, xmax = 160, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Trousers_and_jeans.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shoes and footwear", ymax = 175, y_break = 25, xmax = 170, binwidth = 5, x_break = 10, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shoes_and_footwear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Underwear and socks", ymax = 350, y_break = 25, xmax = 160, binwidth = 5, x_break = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Underwear_and_socks.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Nightwear and homewear", ymax = 60, y_break = 25, xmax = 170, binwidth = 5, x_break = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Nightwear_and_homewear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Accessories", ymax = 170, y_break = 25, xmax = 220, binwidth = 5, x_break = 10, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Accessories.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Sportswear", ymax = 175, y_break = 25, xmax = 130, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Sportswear.png")



#########################################################
## Plot Diary wears (cumulative) histogram by category ##
#########################################################

# Create histogram data manually to include items with 0 wears as its own bin
# REFACTOR to something more elegant :)

# Set histogram bin limits and labels. There are used for correct plot order also
histogram_wear_tiers_bin_max <- c(0, 1, 2, 3, 5, 10, 20, 50, 50)
histogram_wear_tiers_bin_label <- c("0", "1", "2", "3", "4-5", "6-10", "11-20", "21-50", "Over 50")
plot_data <- raw_data %>% select(category, wears) %>% filter(!is.na(wears)) %>%
  mutate(bin = as.factor(case_when(
    wears <= histogram_wear_tiers_bin_max[1] ~ histogram_wear_tiers_bin_label[1],
    wears <= histogram_wear_tiers_bin_max[2] ~ histogram_wear_tiers_bin_label[2],
    wears <= histogram_wear_tiers_bin_max[3] ~ histogram_wear_tiers_bin_label[3],
    wears <= histogram_wear_tiers_bin_max[4] ~ histogram_wear_tiers_bin_label[4],
    wears <= histogram_wear_tiers_bin_max[5] ~ histogram_wear_tiers_bin_label[5],
    wears <= histogram_wear_tiers_bin_max[6] ~ histogram_wear_tiers_bin_label[6],
    wears <= histogram_wear_tiers_bin_max[7] ~ histogram_wear_tiers_bin_label[7],
    wears <= histogram_wear_tiers_bin_max[8] ~ histogram_wear_tiers_bin_label[8],
    wears >  histogram_wear_tiers_bin_max[9] ~ histogram_wear_tiers_bin_label[9]
  )))

# Prepare histogram data, including cumulative counts and shares
plot_data <- plot_data %>%
  group_by(category, bin) %>%
  summarise(items = n()) %>% as.data.frame() %>%
  arrange(factor(bin, levels = histogram_wear_tiers_bin_label)) %>%
  group_by(category) %>%
  mutate(share = items/sum(items)) %>%
  mutate(cum_items = cumsum(items), cum_share = cumsum(share)) %>%
  as.data.frame()

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jackets and coats", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Jackets_and_coats.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jackets and coats", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Jackets_and_coats.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Blazers and vests", ymax = 0.3, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Blazers_and_vests.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Blazers and vests", ymax = 1, ybreak = 0.1, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Blazers_and_vests.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jumpers and hoodies", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Jumpers_and_hoodies.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jumpers and hoodies", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Cardigans and knits", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Cardigans_and_knits.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Cardigans and knits", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Cardigans_and_knits.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shirts and blouses", ymax = 0.3, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Shirts_and_blouses.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shirts and blouses", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Shirts_and_blouses.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "T-shirts and tops", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-T-shirts_and_tops.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "T-shirts and tops", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-T-shirts_and_tops.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Dresses and jumpsuits", ymax = 0.5, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Dresses_and_jumpsuits.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Dresses and jumpsuits", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shorts and skirts", ymax = 0.4, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Shorts_and_skirts.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shorts and skirts", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Shorts_and_skirts.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Trousers and jeans", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Trousers_and_jeans.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Trousers and jeans", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Trousers_and_jeans.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shoes and footwear", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Shoes_and_footwear.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shoes and footwear", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Shoes_and_footwear.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Underwear and socks", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Underwear_and_socks.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Underwear and socks", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Underwear_and_socks.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Nightwear and homewear", ymax = 0.3, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Nightwear_and_homewear.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Nightwear and homewear", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Nightwear_and_homewear.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Accessories", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Accessories.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Accessories", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Accessories.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Sportswear", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Sportswear.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Sportswear", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Sportswear.png")




####################################
## Plot CPW and Wears by category ##
####################################

## TOTAL WEARS

# Prepare plot data for plotting Total wears (including estimates of prior use)
plot_data <- raw_data %>%
  mutate(plot_wears = total_wears + wears) %>% 
  select(category, cpw, plot_wears) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

# Plot all categories # CONTINUE HERE >>>
for (category in category_order) {
  #if (max(plot_data$plot_wears[category == category], na.rm = TRUE) > 500) { xbreak = 50 } else { xbreak = 20 }
  p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = TRUE, xmax = NA, xbreak = 50, ymax = NA)
  filename <- paste("Z-CPW_and_Total_wears-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}


## DIARY WEARS

# Prepare plot data for plotting Diary wears (and calculating CPW base only on those)
plot_data <- raw_data
plot_data$wears[plot_data$wears == 0] <- 1 # Replace all 0 wears with 1 to in effect set CPW to price for these items
plot_data <- plot_data %>%
  mutate(plot_wears = wears, cpw = price/wears) %>% 
  select(category, cpw, plot_wears) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

# Plot all categories
for (category in category_order) {
  if (max(plot_data$plot_wears[category == category]) > 150) { xbreak = 10 } else { xbreak = 5 }
  p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = NA, ymax = NA)
  filename <- paste("Z-CPW_and_Diary_wears-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}








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

# Total number of NAs in price
sum(is.na(raw_data$price))

str(raw_data)

# List which users repaired 1+ item(s)
raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(user) %>% summarise(repaired_items = n(), repaired_value = sum(price, na.rm = TRUE))

# List categories of repaired items
raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(category) %>% summarise(repaired_items = n())


# List number of items by user
raw_data %>% filter(!is.na(item)) %>%
  group_by(user) %>% summarise(total_items = n()) %>%
  arrange(desc(total_items)) %>%
  as.data.frame()

# List total value of items by user
raw_data %>% filter(!is.na(item)) %>%
  group_by(user) %>% summarise(total_value = sum(price, na.rm = TRUE)) %>%
  arrange(desc(total_value)) %>%
  as.data.frame()

# List number of items that need repair by user
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  group_by(user) %>% summarise(total_items = n()) %>%
  as.data.frame()

# List value of items that need repair by user
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  group_by(user) %>% summarise(total_value = sum(price, na.rm = TRUE)) %>%
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



### WEARS PER MONTHS

## Months available

# Functions for calculating the difference in months between two dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
} 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# Set diary period starting date to use in filtering
diary_starting_date <- as.Date("2021-05-23")


# Set the total active tracking time in months
months_tracked <- 3


# Calculate months available for all items
raw_data <- raw_data %>% mutate(months_available = mondf(date_purchased, diary_starting_date))

raw_data$months_available[is.na(raw_data$date_purchased)] 

# List wears_per_month as estimated by users
raw_data %>% select(category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  filter(category == "T-shirts and tops") %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / months_tracked, digits = 1)) %>%
  arrange(desc(WPM_est)) %>%
  head(20)

# List wears_per_month for specific user and category
raw_data %>% select(user, category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  filter(category == "Shoes and footwear", user == "Quirine") %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / months_tracked, digits = 1)) %>%
  arrange(desc(WPM_est)) %>%
  head(20)



# List top wears_per_month during diary period
raw_data %>% select(user, category, item, wears, total_wears, months_available) %>%
  filter(months_available > 0) %>%
  mutate(wears_per_month = wears / months_tracked) %>% # 5 weeks diary period = 1.25 months
  arrange(desc(wears_per_month))


## Summarise wears_per_month by user and category

# Set max WPM to every day (avg 30.5 days per month)
WPM_max = 30.5

# Calculate deltas
WPM_delta <-
  raw_data %>% select(user, category, item, months_available, wears_real = wears, wears_est = total_wears) %>%
  filter(months_available > 0) %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1), WPM_real = round(wears_real / months_tracked, digits = 1)) %>%
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
  filter(category %in% category_order[9]) %>%
  setup_WPM_delta_plot_categories(xmax = 7, ymax = 4, wpm_max = 1.0)
p
ggsave(filename = "Plots/Z/WPM-Estimate_vs_max_and_real-example.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("WPM-Estimate_vs_max_and_real-example.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)




### Heatmaps

# Remove unfit categories and users with incomplete data
WPM_delta_hm <- WPM_delta %>%
  filter(!(category %in% c("Underwear and socks", "Nightwear and homewear", "Accessories", "Sportswear", "Other"))) %>%
  filter(!user == "Melanie")
  

## Heatmap of WPM delta to max by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$delta_to_max, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=1 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>1 & breaks[-1]<=1.2 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>1.2 & breaks[-1]<=2 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>2), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)

# Build heatmap
WPM_delta_hm %>% 
  select(user, category, delta_to_max) %>%
  spread("category", delta_to_max) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE) %>%
  heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "User-estimated WPM compared to max (1x)",
            key.title = FALSE, key.xlab = "Multiplier of max")

## Heatmap of WPM delta to real by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$delta_to_real, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=1 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>1 & breaks[-1]<=2 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>2 & breaks[-1]<=15 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>15), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)

# Build heatmap
WPM_delta_hm %>% 
  select(user, category, delta_to_real) %>%
  spread("category", delta_to_real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE) %>%
  heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "User-estimated WPM compared to real (1x)",
            key.title = FALSE, key.xlab = "Multiplier of real")



## Heatmap of WPM real by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$real, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=15 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>15 & breaks[-1]<=30.5 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>30.5 & breaks[-1]<=61 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>61), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)

# Build heatmap
WPM_delta_hm %>% 
  select(user, category, real) %>%
  spread("category", real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE) %>%
  heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "Real WPM during diary period",
            key.title = FALSE, key.xlab = "Category WPM")


# Real WPM with color scaling
WPM_delta_hm %>% 
  select(user, category, real) %>%
  spread("category", real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE) %>%
  heatmap.2(scale = "row", trace = "none", density.info = "none",
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "Real WPM during diary period",
            key.title = FALSE, key.xlab = "Category WPM")



### DIVESTEMENTS

# Number of divested items
sum(!is.na(raw_data$date_divested))

# List of divested items
raw_data[!is.na(raw_data$date_divested),]

# List divestment way
raw_data %>%
  filter(!is.na(date_divested)) %>%
  group_by(divestment_way) %>%
  summarise(items_divested = n()) %>%
  mutate(share_of_total = round(items_divested / sum(items_divested), digits = 2)) %>%
  arrange(desc(items_divested)) %>%
  as.data.frame()

# List item condition when divested
raw_data %>%
  filter(!is.na(date_divested)) %>%
  group_by(condition) %>%
  summarise(items_divested = n()) %>%
  mutate(share_of_total = round(items_divested / sum(items_divested), digits = 2)) %>%
  arrange(desc(items_divested)) %>%
  as.data.frame()

# Create matrix of the two above
raw_data %>%
  filter(!is.na(date_divested)) %>%
  group_by(divestment_way, condition) %>%
  summarise(items_divested = n()) %>%
  #mutate(share_of_total = round(items_divested / sum(!is.na(raw_data$date_divested)), digits = 2)) %>%
  pivot_wider(names_from = divestment_way, values_from = items_divested)
  



###################################################################################################
######################################## PLOT FUNCTIONS ###########################################
######################################## PLOT FUNCTIONS ###########################################
######################################## PLOT FUNCTIONS ###########################################
######################################## PLOT FUNCTIONS ###########################################
######################################## PLOT FUNCTIONS ###########################################
###################################################################################################

## Function: Item price distribution by category
setup_price_distribution_plot <- function(plot_data, categories, xmax = NA, ymax = NA, binwidth = 10, legend = TRUE, x_break = 10, repel_gap = 0) {
  
  # Filter data by category
  if(!is.na(categories)) {
    plot_data <- plot_data %>% filter(category %in% categories)  
  }
  
  # Remove items with no price data
  plot_data <- plot_data %>% filter(!is.na(price))
    
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$price, na.rm = TRUE) }
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



## Function: Number of wardrobe items vs share of items worn worn_count+ times
setup_share_worn_plot <- function(plot_data, categories, xmax = NA, ymax = 1, trendline = TRUE, worn_count = 1) {
  
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
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size))
  
  if(trendline) { p <- p + geom_smooth(method = "lm") }
  
  p <- p + geom_hline(yintercept = mean(plot_data$share_worn, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = mean(plot_data$share_worn, na.rm=TRUE), label=paste("Mean\n", round(mean(plot_data$share_worn*100, na.rm=TRUE), digits = 0), "%")), color =  "darkgrey", fill = "white") +
    geom_vline(xintercept = mean(plot_data$items, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$items, na.rm=TRUE), y = 0, label=paste("Mean\n", round(mean(plot_data$items, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = 2)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = 0.1), labels = scales::percent_format(accuracy = 1L)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    #scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE)
  
  if (worn_count == 1) {
    p <- p + labs(x = "Amount of items in wardrobe", y = "Share of wardrobe items worn at least once during the diary period") +
      ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)")
    
  } else {
    p <- p + labs(x = "Amount of items in wardrobe", y = paste("Share of wardrobe items worn at least ", worn_count, " times during the diary period", sep = "")) +
      ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)") +
      geom_label(aes(x = 2, y = 0.1 , label=paste("Worn at least\n", worn_count, " times", sep = "")), color =  "darkred", fill = "white")
      
  }
  
  return(p)
}



## Function: Item Diary wears distribution by category
setup_Diary_wears_distribution_plot <- function(plot_data, categories = NA, xmax = NA, ymax = NA, y_break = 25, binwidth = 10, legend = TRUE, x_break = 10, repel_gap = 0) {
  
  # Filter data by category
  if(!is.na(categories)) {
    plot_data <- plot_data %>% filter(category %in% categories)  
  }
  
  # Remove items with no price data
  plot_data <- plot_data %>% filter(!is.na(wears))
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$wears, na.rm = TRUE) }
  #  if(is.na(ymax)) { ymax <- max(plot_data$average_value) }
  author_label_x <- xmax
  author_label_y <- ymax
  
  p <-ggplot(
    plot_data,
    aes(x = wears, fill = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_histogram(binwidth = binwidth, boundary = 0) +
    geom_vline(xintercept = mean(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$wears + repel_gap, na.rm=TRUE), y = 0, label=paste("Mean\n", round(mean(plot_data$wears, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
    geom_vline(xintercept = median(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$wears - repel_gap, na.rm=TRUE), y = 0, label=paste("Median\n", round(median(plot_data$wears, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = x_break)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = y_break)) +
    scale_fill_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    labs(x = "Diary wears", y = "Total number of items by Diary wears tier") +
    ggtitle("Item Diary wears distribution by category (all users)")
  
  if (!legend){ theme(legend.position = "none") }
  
  return(p)
}




# Function: to setup category point plot y = purchase price, x = times worn
setup_category_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 1, ybreak = 10, ybreaks = plot_log_breaks, log_trans=TRUE, avg_lines=TRUE, trendline = TRUE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$wears, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$price, na.rm = TRUE) }
  
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
    scale_x_continuous(breaks = seq.int(from = 0, to = xmax, by = xbreak), limits=c(0,xmax)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Diary wears", y = "Purchase price (€)")
    
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks, labels=scales::dollar_format(suffix = "€", prefix = "")) }
  else { p <- p + scale_y_continuous(breaks = seq.int(from = 0, to = ymax, by = ybreak), limits=c(NA,ymax), labels=scales::dollar_format(suffix = "€", prefix = "")) }

  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  if (avg_lines) { p <- p +
    geom_vline(xintercept = median(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$wears, na.rm=TRUE), y = min(plot_data$wears, na.rm=TRUE)+0.05, label=round(median(plot_data$wears, na.rm=TRUE), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$price, na.rm=TRUE), label=paste(round(median(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey")
  }
    
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
#    geom_vline(xintercept = mean(plot_data$average_items), color="darkgrey", linetype="dashed") +
#    geom_label(aes(x = mean(plot_data$average_items), y = min(plot_data$average_value), label=round(mean(plot_data$average_items), digits = 0)), color =  "darkgrey") +
#    geom_hline(yintercept = mean(plot_data$average_value), color="darkgrey", linetype="dashed") +
#    geom_label(aes(x = 0, y = mean(plot_data$average_value), label=paste(round(mean(plot_data$average_value), digits = 0), "€")), color =  "darkgrey") +
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
    scale_color_manual(name = "Users", values = gender_colors) +
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


# Function: item CPW and wears by category (across all users)
setup_CPW_and_Wears_plot <- function(plot_data, categories, plot_total_wears = TRUE, xmax = NA, xbreak = 10, ymax = NA, ybreak = 25, log_trans = TRUE, trendline = FALSE, avg_lines = TRUE, guides = TRUE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$plot_wears, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$cpw, na.rm = TRUE) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5

  
  # Build guides data
  if(guides){
    # Set xmax for guides to data limit if not defined (in the function call (i.e. set automatically by ggplot)
    if (!is.na(xmax)){ guides_length <- xmax }
    else { guides_length = max(plot_data$plot_wears, na.rm = TRUE)}
    
    # Initiate guides data frame
    guides_data <- data.frame()
    
    # Create guides with 400 observations for each item (purchase price)
    for (i in guides_prices){ # guides_prices is a global variable assumed set
      guides_temp = data.frame(price = rep(i, guides_length))
      guides_temp$cumuse <- seq(1, guides_length, 1)
      guides_temp$cpw <- as.numeric(i) / guides_temp$cumuse
      guides_data <- rbind(guides_data, guides_temp)
    }
    
    # Remove guide data with cost_per_use lower than the plot data, to avoid impact on axis limits
    guides_data <- guides_data[guides_data$cpw >= min(plot_data$cpw, na.rm = TRUE),]
    
    # Create data set of label coordinates to be plotted
    guides_labels_data <- guides_data %>%
      group_by(price) %>%
      summarise(cpw = min(cpw), cumuse = max(cumuse))
  }
  
    
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = plot_wears, y = cpw, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1)
  
  # Add guide trajectories for number of purchase prices (labels are added last)
  if (guides){ p <- p + geom_point(data = guides_data, aes(x = cumuse, y = cpw), colour = "lightblue", size = 0.4) }
    
  p <- p + geom_point(show.legend = FALSE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(limits=c(NA,xmax), breaks = seq.int(from = 0, to = xmax, by = xbreak)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE)
  
  if (plot_total_wears) {
    p <- p + labs(x = "Total wears (including estimates prior to diary)", y = "Cost Per Wear (CPW)") +
      ggtitle("Cost Per Wear and (estimated) total wears by category (across all users)")
  } else {
    p <- p + labs(x = "Diary wears", y = "Cost Per Wear (CPW) excluding wears before diary") +
      ggtitle("Cost Per Wear and Diary wears by category (across all users)")
  }
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), labels=scales::dollar_format(suffix =" €", prefix = "")) }
  else { p <- p + scale_y_continuous(limits=c(NA,ymax), breaks = seq.int(from = 0, to = ymax, by = ybreak), labels=scales::dollar_format(suffix = "€", prefix = "")) }
  
  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  # Add average lines and labels
  if (avg_lines) { p <- p +
    geom_vline(xintercept = median(plot_data$plot_wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$plot_wears, na.rm=TRUE), y = min(plot_data$cpw, na.rm=TRUE), label=round(median(plot_data$plot_wears, na.rm=TRUE), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$cpw, na.rm = TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$cpw, na.rm = TRUE), label=paste(round(median(plot_data$cpw, na.rm = TRUE), digits = 2), "€")), color =  "darkgrey")
  }

  # Add guide trajectory labels so as to end up on top of other graphical elements
  if (guides){ p <- p + geom_label(data = guides_labels_data, aes(x = cumuse, y = cpw, label=paste(price, "€")), color = "lightblue") }

  return(p)
}

## Function: Item Diary wears custom histogram by category
setup_Diary_wears_histogram_plot <- function(plot_data, categories = NA, ymax = 1, ybreak = 0.1, cumulative = FALSE, legend = TRUE) {
  
  # Filter data by category
  if(!is.na(categories)) { plot_data <- plot_data %>% filter(category %in% categories) }
  
  # Draw plot based on data type
  if(cumulative) {
    p <-ggplot(plot_data,
               aes(x = factor(bin, level = histogram_wear_tiers_bin_label), y = cum_share, fill = category)) +
      geom_col() +
      annotate("text", x = histogram_wear_tiers_bin_label[1], y = ymax, label = author_label, color = "gray", hjust = 0)
  }
  else {
    p <-ggplot(plot_data,
               aes(x = factor(bin, level = histogram_wear_tiers_bin_label), y = share, fill = category)) +
      geom_col() +
      annotate("text", x = histogram_wear_tiers_bin_label[length(histogram_wear_tiers_bin_label)], y = ymax, label = author_label, color = "gray", hjust = 1)
  }
  
  # Finish standard plot components
  p <- p +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = ybreak)) +
    scale_fill_manual(name = "Category", values = category_colors[match(categories, category_order)])
  
  # Add title and share percentages to column bases
  if(cumulative) {
    p <- p + labs(x = "Diary wears", y = "Cumulative share of items in Diary wears tier") +
      ggtitle("Item Diary wears tier cumulative distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.05, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA)
  }
  else {
    p <- p + labs(x = "Diary wears", y = "Share of items in Diary wears tier") +
      ggtitle("Item Diary wears tier distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.01, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA)
  }

  if (!legend){ theme(legend.position = "none") }
  
  return(p)
}







