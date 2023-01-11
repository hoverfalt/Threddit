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
library(circlize) # Enable circular plots 


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
raw_data <- temp_storage

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

# Save raw_data data.frame to file for easier retrieval (2022-09-13)
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

# List non-excluded users
users <- sort(unique(as.character(raw_data$user)))
users <- users[!(users %in% excluded_users)]
str(users)


## Correct categories according to reshuffle in Nov 2022

temp <- raw_data
raw_data <- temp


data_file = get_Google_sheet_ID_Z3()
category_changes <- read_sheet(data_file, sheet='Category change log')
str(category_changes)

# Change categories
for (i in 1:nrow(category_changes)) {
  raw_data$category[raw_data$user == category_changes$user[i] & raw_data$item == category_changes$item[i]] <-
    category_changes$target_category[i]
}

# Remove glasses and sunglasses
raw_data <- raw_data %>% filter(!(category == "Accessories" & grepl("glasses", item)))

raw_data %>% filter(category == "Accessories" & grepl("glasses", item)) %>%
  select(user, category, item, price, wears) %>% arrange(desc(wears))

# Remove category "Other"
raw_data <- raw_data %>% filter(category != "Other")


# Remove excluded users and previously divested items (83) from raw data
raw_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested) | date_divested >= "2021-09-01")

nrow(raw_data)
unique(raw_data$category)

#################################################################################################
######################################## SET UP PLOTS ###########################################
#################################################################################################


## Average wardrobe size and value by user

# Filter data to include all items currently active (not divested)
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(user) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  as.data.frame()

total_data <- merge(plot_data, user_data, by = c("user"))

p <- total_data %>% setup_user_distribution_plot(xmax = 320, xbreak = 20, ymax = 15000, ybreak = 1000)
#p <- p + geom_smooth(method = "lm", se = FALSE)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_user.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_user.png")

# Check statistical significance for correlation between wardrobe value and number of items
wardrobes_model <- lm(value ~ items, data = total_data[total_data$gender == "Female",])
summary(wardrobes_model)


# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)

# Count of items
nrow(raw_data)

# Total value of items
sum(raw_data$price, na.rm = TRUE)



# List share of secondhand by user
secondhand_by_user <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(user, category, item, secondhand, divestment_way, date_purchased, date_divested) %>%
  mutate(acquired_item = as.logical(as.Date(date_purchased) > as.Date("2021-09-01"))) %>%
  mutate(divested_item = as.logical(as.Date(date_divested) > as.Date("2021-09-01"))) %>%
  select(user, category, item, secondhand, acquired_item, divested_item, divestment_way) %>%
  filter(!is.na(acquired_item)) %>%
  group_by(user, acquired_item) %>%
  summarise(items = n(), secondhand_items = sum(secondhand), secondhand_share = round(secondhand_items / items, digits = 2)) %>%
  pivot_wider(names_from = acquired_item, values_from = c(items, secondhand_items, secondhand_share)) %>%
  as.data.frame() %>%
  select(user, old_items = items_FALSE, old_SH_items = secondhand_items_FALSE, old_SH_share = secondhand_share_FALSE,
         new_items = items_TRUE, new_SH_items = secondhand_items_TRUE, new_SH_share = secondhand_share_TRUE) %>%
  mutate(SH_share_change = new_SH_share - old_SH_share) %>%
  arrange(desc(old_SH_share))

write.csv(secondhand_by_user,"Plots/Z/Z-Secondhand_share_by_user.csv", row.names = FALSE)


  
## Average wardrobe size and value by category

plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  mutate(average_items = items/length(unique(raw_data$user)), average_value = value / items)

p <- plot_data %>% setup_category_distribution_plot(categories = category_order, xmax = 30, xbreak = 5, ymax = 90, legend  = FALSE)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_category.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_category.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)



## Average wardrobe size and value by category by user

plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(user, category) %>%
  summarise(items = n(), value = sum(price, na.rm = TRUE))

p <- plot_data %>% setup_category_distribution_plot_by_user(users = "Vanessa", categories = category_order, xmax = NA, xbreak = 5, ymax = NA, legend  = FALSE)
ggsave(filename = "Plots/Z/Z-Average_wardrobe_size_and_value_by_category.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Average_wardrobe_size_and_value_by_category.png")

# Plot all users and all categories except Other
for (user in users) {
  p <- plot_data %>% filter(!(category %in% "Other")) %>%
    setup_category_distribution_plot_by_user(users = user, categories = category_order, xmax = NA, xbreak = 5, ymax = NA, legend  = FALSE)
  filename <- paste("Z-Wardrobe_size_and_value_by_user-", gsub(" ", "_", user), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}


# Create table of category items and value by user

plot_data <- raw_data %>%
  select(user, category, item, price, date_purchased, date_divested) %>%
  #filter(is.na(date_divested)) %>%
  filter(date_purchased <= "2021-09-01" | is.na(date_purchased)) %>%
  group_by(user, category) %>%
  summarise(items = n(), value = sum(price, na.rm = TRUE)) %>%
  select(user, category, items) %>%
  spread(category, items) %>%
  as.data.frame()
write.csv(plot_data,"Plots/Z/Z-Category_size_by_user-END.csv", row.names = FALSE)
write.csv(plot_data,"Plots/Z/Z-Category_value_by_user-END.csv", row.names = FALSE)

nrow(plot_data)

plot_data <- raw_data %>%
  select(user, category, item, price, date_purchased, date_divested) %>%
  mutate(new_item = as.logical(as.Date(date_purchased) > as.Date("2021-09-01"))) %>%
  mutate(divested_item = as.logical(as.Date(date_divested) > as.Date("2021-09-01"))) %>%
  mutate(new_item_value = new_item * price, divested_item_value = divested_item * price) %>%
  rowwise() %>%
  group_by(user, category) %>%
  summarise(items_at_start = n() - sum(new_item, na.rm = TRUE),
            new_items = sum(new_item, na.rm = TRUE),
            divested_items = sum(divested_item, na.rm = TRUE),
            items_at_end = n() - sum(divested_item, na.rm = TRUE),
            value_at_start = round(sum(price, na.rm = TRUE) - sum(new_item_value, na.rm = TRUE), digits = 0),
            value_new_items = round(sum(new_item_value, na.rm = TRUE), digits = 0),
            value_divested_items = round(sum(divested_item_value, na.rm = TRUE), digits = 0),
            value_at_end = round(sum(price, na.rm = TRUE) - sum(divested_item_value, na.rm = TRUE), digits = 0)) %>%
  select(user, category, items_at_start, items_at_end, new_items, divested_items, 
         value_at_start, value_new_items, value_divested_items, value_at_end) %>%
  as.data.frame()
table_data <- plot_data %>%
  select(user, category, value_at_start) %>%
  spread(category, value_at_start) %>%
  as.data.frame()

write.csv(table_data,"Plots/Z/Z-Category_size_by_user-START.csv", row.names = FALSE)
write.csv(table_data,"Plots/Z/Z-Category_value_by_user-START.csv", row.names = FALSE)





## Wardrobe change: new and divested items count and value by user
str(raw_data)
plot_data <- raw_data %>%
  select(user, category, item, price, date_purchased, date_divested) %>%
  mutate(new_item = as.logical(as.Date(date_purchased) > as.Date("2021-09-01"))) %>%
  mutate(divested_item = as.logical(as.Date(date_divested) > as.Date("2021-09-01"))) %>%
  mutate(new_item_value = new_item * price, divested_item_value = divested_item * price) %>%
  rowwise() %>%
  group_by(user) %>%
  summarise(items_at_start = n() - sum(new_item, na.rm = TRUE),
            new_items = sum(new_item, na.rm = TRUE),
            divested_items = sum(divested_item, na.rm = TRUE),
            items_at_end = n() - sum(divested_item, na.rm = TRUE),
            value_at_start = round(sum(price, na.rm = TRUE) - sum(new_item_value, na.rm = TRUE), digits = 0),
            value_new_items = round(sum(new_item_value, na.rm = TRUE), digits = 0),
            value_divested_items = round(sum(divested_item_value, na.rm = TRUE), digits = 0),
            value_at_end = round(sum(price, na.rm = TRUE) - sum(divested_item_value, na.rm = TRUE), digits = 0)) %>%
  mutate(change = new_items - divested_items,
         change_percent = round((new_items - divested_items) / items_at_start, digits = 2),
         share_new = round(new_items / items_at_start, digits = 2),
         share_divested = round(divested_items / items_at_start, digits = 2),
         change_value = value_new_items - value_divested_items,
         share_new_value = round(value_new_items / value_divested_items), digits = 0) %>%
  select(user, items_at_start, items_at_end, new_items, share_new, divested_items, share_divested, change, change_percent,
         value_at_start, value_new_items, value_divested_items, value_at_end) %>%
  as.data.frame()

write.csv(plot_data,"Plots/Z/Z-Wardrobe_change_by_user.csv", row.names = FALSE)



## List highest value items across users
valuable_items <- raw_data %>% select(user, category, item, price) %>%
  filter(price >= 100) %>%
  filter(category == "Accessories" | category == "Other") %>%
  arrange(desc(price))
write.csv(valuable_items,"Plots/Z/Z-Valuable_items.csv", row.names = FALSE)





## Calculate user wardrobe key metrics
# Items at end of study
# Total wardrobe value at end of study
# Average item value at end of study
# % net items increase over study
# % net value increase over study
wardrobes <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  group_by(user) %>%
  summarise(items = n(), value = round(sum(price, na.rm = TRUE), digits = 0)) %>%
  mutate(items_delta = round((items - mean(items)) / mean(items), digits = 2),
         value_delta = round((value - mean(value)) / mean(value), digits = 2),
         avg_item_value = round(value / items, digits = 2)) %>%
  mutate(avg_item_value_delta = round((avg_item_value - mean(avg_item_value)) / mean(avg_item_value), digits = 2),
         user = as.character(user)) %>%
  select(user, items, items_delta, value, value_delta, avg_item_value, avg_item_value_delta) %>%
  as.data.frame() %>%
  arrange(user)

# DEPENDENCY: Uses plot_data from previous calculation 
wardrobes_2 <- plot_data %>% select(user, new_items, divested_items, items_change = change_percent, value_at_start, value_at_end) %>%
  mutate(user = as.character(user),
         net_new_items = new_items - divested_items, 
         value_change = value_at_end - value_at_start,
         value_change_percent = round((value_at_end - value_at_start) / value_at_start, digits = 2)) %>%
  select(user, net_new_items, items_change, value_change, value_change_percent) %>%
  as.data.frame() %>%
  arrange(user)

merge(wardrobes, wardrobes_2)



# Calculate average item price across all users and categories
raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  group_by(user) %>%
  summarise(items = n(), value = round(sum(price, na.rm = TRUE), digits = 0)) %>%
  mutate(avg_item_value = round(value / items, digits = 2)) %>%
  mutate(avg_item_value_overall = mean(avg_item_value))
  
mean(wardrobes_2$items_change)
mean(wardrobes_2$value_change)

# Show price variance by category 
item_value_by_category <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), value = sum(price, na.rm = TRUE), mean = mean(price, na.rm = TRUE), sd = sd(price, na.rm = TRUE)) %>%
  mutate(coefficient_of_variation = sd / mean) %>%
  arrange(desc(coefficient_of_variation))

write.csv(item_value_by_category,"Plots/Z/Z-Item_value_by_category.csv", row.names = FALSE)


## List short-lived items; items purchased and divested within the 12-month study period
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(date_purchased >= "2021-09-01" & !is.na(date_divested)) %>%
  select(user, category, item, price, wears, secondhand, date_purchased, date_divested, divestment_way, wears_per_wash, condition) %>%
  mutate(days_in_use = date_divested - date_purchased) %>%
  arrange(user, date_purchased)
write.csv(plot_data,"Plots/Z/Z-Short_lived_items.csv", row.names = FALSE)






## Item price distribution by category

# Item price distribution 
#p <- raw_data %>% setup_price_distribution_plot(categories = category_order, xmax = 200, ymax = 1250, binwidth = 10, x_break = 20, repel_gap = 5)
#ggsave(filename = "Plots/Z/Z-Item_price_distribution.png", p, width = 9, height = 7, dpi = 150, units = "in")
#save_to_cloud_Z("Z-Item_price_distribution.png")

p <- raw_data %>% filter(!(user %in% excluded_users)) %>% filter(is.na(date_divested)) %>%
  setup_price_distribution_plot(categories = c("Jackets and coats"), xmax = 360, ymax = 35, binwidth = 10, x_break = 20, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jackets_and_coats.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Jackets and coats", price > 360)


p <- raw_data %>% filter(!(user %in% excluded_users)) %>% filter(is.na(date_divested)) %>%
  setup_price_distribution_plot(categories = c("Blazers and vests"), xmax = 300, ymax = 25, binwidth = 10, x_break = 50, repel_gap = 8)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Blazers_and_vests.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Blazers and vests", price > 300)

p <- raw_data %>% filter(!(user %in% excluded_users)) %>% filter(is.na(date_divested)) %>%
  setup_price_distribution_plot(categories = c("Jumpers and hoodies"), xmax = 130, ymax = 60, binwidth = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Jumpers_and_hoodies.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Jumpers and hoodies", price > 130)

p <- raw_data %>% filter(!(user %in% excluded_users)) %>% filter(is.na(date_divested)) %>%
  setup_price_distribution_plot(categories = c("Cardigans and knits"), xmax = 425, ymax = 65, binwidth = 10, x_break = 25, repel_gap = 12)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Cardigans_and_knits.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Cardigans and knits", price > 425)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Shirts and blouses"), xmax = 230, ymax = 100, binwidth = 10, x_break = 20, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shirts_and_blouses.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Shirts and blouses", price > 230)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("T-shirts and tops"), xmax = 150, ymax = 340, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-T-shirts_and_tops.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "T-shirts and tops", price > 130)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 250, ymax = 60, binwidth = 10, x_break = 20, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Dresses_and_jumpsuits.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Dresses and jumpsuits", price > 250)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Shorts and skirts"), xmax = 240, ymax = 70, binwidth = 10, x_break = 20, repel_gap = 8)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shorts_and_skirts.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Shorts and skirts", price > 220)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Trousers and jeans"), xmax = 275, ymax = 85, binwidth = 10, x_break = 20, repel_gap = 8)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Trousers_and_jeans.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Trousers and jeans", price > 275)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Shoes and footwear"), xmax = 475, ymax = 65, binwidth = 10, x_break = 25, repel_gap = 15)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Shoes_and_footwear.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Shoes and footwear", price > 475)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Underwear and socks"), xmax = 160, ymax = 420, binwidth = 10, repel_gap = 5)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Underwear_and_socks.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Underwear and socks", price > 160)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Nightwear and homewear"), xmax = 130, ymax = 90, binwidth = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Nightwear_and_homewear.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Nightwear and homewear", price > 130)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Accessories"), xmax = 360, ymax = 110, binwidth = 10, x_break = 25, repel_gap = 8)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Accessories.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Accessories", price > 360)

p <- raw_data %>% filter(is.na(date_divested)) %>% setup_price_distribution_plot(categories = c("Sportswear"), xmax = 210, ymax = 110, binwidth = 10, repel_gap = 7)
ggsave(filename = "Plots/Z/Z-Item_price_distribution_by_category-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Item_price_distribution_by_category-Sportswear.png")
raw_data %>% filter(!(user %in% excluded_users), is.na(date_divested)) %>% select(user, category, item, price) %>%
  filter(category == "Sportswear", price > 200)


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
  summarise(items = n(), share_worn = sum(worn)/n()) %>%
  as.data.frame()

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
#  mutate(worn = as.logical(wears == 0)) %>%
  mutate(worn = as.logical(wears >= 1 & wears < worn_count)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

# Plot all categories
for (category in category_order) {
  p <- plot_data %>% setup_share_worn_plot(categories = category, worn_count = worn_count)
  filename <- paste("Z-Wardrobe_items_and_share_worn-n-times", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}


# Check for statistical significance
model_data_sample <- plot_data %>% filter(category == "Trousers and jeans")
wears_model <- lm(share_worn ~ items, data = model_data_sample)
summary(wears_model)
summary(wears_model)$coefficient



## List number of items by category by user
plot_data <- raw_data %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n()) %>%
  spread(category, items) %>%
  as.data.frame()
write.csv(plot_data,"Plots/Z/Z-Amount_of_active_items_by_categoty_and_user.csv", row.names = FALSE)


## List number of unused and rarely used items by category by user
worn_count <- 4
rarely_used_shares <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(!category %in% c("Underwear and socks", "Other")) %>%
#  filter(is.na(date_divested)) %>%
  mutate(worn_1 = as.logical(wears >= 1)) %>%
  mutate(worn_n = as.logical(wears >= worn_count)) %>%
  group_by(user) %>%
  summarise(items = n(), worn_1 = sum(worn_1), worn_n = sum(worn_n), share_unworn = 1 - sum(worn_1)/n(), share_rarely_worn = 1 - sum(worn_n)/n()) %>%
  mutate(share_unworn = round(share_unworn, 2), share_rarely_worn = round(share_rarely_worn, 2)) %>%
  as.data.frame()
write.csv(rarely_used_shares,"Plots/Z/Z-Share_of_unused_and_rarely_used_items_by_participant.csv", row.names = FALSE)

# Overall share worn once and n times
1 - (sum(rarely_used_shares$worn_1) / sum(rarely_used_shares$items))
1 - (sum(rarely_used_shares$worn_n) / sum(rarely_used_shares$items))


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
  filter(!(user %in% excluded_users)) %>%
  #filter(is.na(date_divested)) %>%
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

p <- plot_data %>% setup_category_plot(categories = "Jackets and coats", log_trans=FALSE, xmax = 60, xbreak=5, ymax = 200)
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


p <- plot_data %>% setup_category_plot(categories = "Dresses and jumpsuits", log_trans=FALSE, xmax = NA, xbreak=10, ymax = NA, ybreak = 25)
ggsave(filename = "Plots/Z/Z-Price_and_Diary_wears-Dresses_and_jumpsuits-all.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Price_and_Diary_wears-Dresses_and_jumpsuits-all.png")

p <- plot_data %>% setup_category_plot(categories = "Dresses and jumpsuits", log_trans=FALSE, xmax = 30, xbreak=5, ymax = 125, ybreak = 25)
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

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jackets and coats", ymax = 110, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Jackets_and_coats.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Blazers and vests", ymax = 85, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Blazers_and_vests.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jumpers and hoodies", ymax = 100, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Cardigans and knits", ymax = 190, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Cardigans_and_knits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shirts and blouses", ymax = 275, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shirts_and_blouses.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "T-shirts and tops", ymax = 425, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-T-shirts_and_tops.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Dresses and jumpsuits", ymax = 250, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shorts and skirts", ymax = 160, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shorts_and_skirts.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Trousers and jeans", ymax = 200, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Trousers_and_jeans.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shoes and footwear", ymax = 200, y_break = 25, xmax = NA, binwidth = 5, x_break = 10, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Shoes_and_footwear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Underwear and socks", ymax = 325, y_break = 25, xmax = NA, binwidth = 5, x_break = 25, repel_gap = 10)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Underwear_and_socks.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Nightwear and homewear", ymax = 60, y_break = 25, xmax = NA, binwidth = 5, x_break = 10, repel_gap = 4)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Nightwear_and_homewear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Accessories", ymax = 170, y_break = 25, xmax = NA, binwidth = 5, x_break = 10, repel_gap = 2)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Accessories.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Sportswear", ymax = 175, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/Z/Z-Diary_wears_distribution-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_distribution-Sportswear.png")


# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


#########################################################
## Plot Diary wears (cumulative) histogram by category ##
#########################################################

## Create histogram data manually to include items with 0 wears as its own bin

# Set histogram bin limits and labels. There are used for correct plot order also
histogram_wear_tiers_bin_max <- c(0, 1, 2, 3, 5, 10, 20, 50, 50)
histogram_wear_tiers_bin_label <- c("0", "1", "2", "3", "4-5", "6-10", "11-20", "21-50", "Over 50")
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(category, wears) %>% filter(!is.na(wears)) %>%
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

plot_data %>% select(category, bin, items) %>% spread(category, items) %>%
write.csv("Plots/Z/Z-Diary_wears_histogram_ITEMS.csv", row.names = FALSE)


write.csv(plot_data,"Plots/Z/Z-Diary_wears_histogram_by_categoty.csv", row.names = FALSE)


p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jackets and coats", ymax = 0.25, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Jackets_and_coats.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jackets and coats", ymax = 1, ybreak = 0.10, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Jackets_and_coats.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Blazers and vests", ymax = 0.35, ybreak = 0.05)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-Blazers_and_vests.png")
p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Blazers and vests", ymax = 1, ybreak = 0.1, cumulative = TRUE)
ggsave(filename = "Plots/Z/Z-Diary_wears_histogram-cumulative-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("Z-Diary_wears_histogram-cumulative-Blazers_and_vests.png")

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Jumpers and hoodies", ymax = 0.30, ybreak = 0.05)
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

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Shorts and skirts", ymax = 0.45, ybreak = 0.05)
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

p <- plot_data %>% setup_Diary_wears_histogram_plot(categories = "Underwear and socks", ymax = 0.30, ybreak = 0.05)
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
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.05, label=scales::percent(cum_share, accuracy = 1L)), color =  "white", label.size = NA)
  }
  else {
    p <- p + labs(x = "Diary wears", y = "Share of items in Diary wears tier") +
      ggtitle("Item Diary wears tier distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.01, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA)
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.5, 0.9))
  }
  
  return(p)
}




######################################################################
######### Plot Diary wears distribution by category and user #########
######################################################################

# Select and filter input data
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(user, category, item, wears) %>%
  arrange(user, category, desc(wears))

# Initiate index with zero as default
plot_data$index <- 0

# Set index per user and category
for (user in unique(plot_data$user)) {
  for (category in unique(plot_data$category)) {
    if (nrow(plot_data[plot_data$user == user & plot_data$category == category,]) > 0) {
      plot_data[plot_data$user == user & plot_data$category == category,]$index <-
        seq.int(nrow(plot_data[plot_data$user == user & plot_data$category == category,]))
    }
  }
}


# Cap wears at 150 to make graphs comparable
plot_data$wears[plot_data$wears > 150] <- 150

## Plot Diary wears distribution by category plots for all users
for (user in unique(plot_data$user)) {
  p <- plot_data %>%
    filter(!(category %in% c("Accessories", "Other"))) %>%
    setup_wears_distribution_user_plot(user = user, ymax = 150, y_break = 50, xmax = 30, x_break = 5, legend = TRUE) +
    facet_grid(rows = vars(category))

  filename <- paste("Z-Diary_wears_distribution_by_user_", user, ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

## Function: Item Diary wears distribution by category
setup_wears_distribution_user_plot <- function(plot_data, users, xmax = NA, x_break = 10, ymax = NA, y_break = 100, legend = FALSE) {
  
  # Filter data by user
  if(!is.na(users)) { plot_data <- plot_data %>% filter(user %in% users) }
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$index, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$wears, na.rm = TRUE) }
  author_label_x <- xmax
  author_label_y <- ymax
  
  p <-ggplot(
    plot_data,
    aes(x = index, y = wears, fill = category)) +
    geom_bar(stat="identity") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = x_break)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = y_break)) +
    scale_fill_manual(name = "Category", values = category_colors) +
    labs(x = "Items in decreasing order of wears", y = "Diary wears") +
    ggtitle("Item Diary wears distribution by category")
  
  if (!legend){ p <- p + theme(legend.position = "none") }
  
  return(p)
}


# List Diary wears by category by user
wears_by_category_by_user <- plot_data %>% group_by(user, category) %>%
  summarise(total_wears = sum(wears)) %>%
  arrange(desc(total_wears)) %>%
  spread(category, total_wears) %>%
  as.data.frame()
write.csv(wears_by_category_by_user,"Plots/Z/Z-Diary_wears_by_categoty_by_user.csv", row.names = FALSE)

# List average wears by category
plot_data %>% group_by(category) %>% summarise(items = n(), avg_wears = mean(wears), med_wears = median(wears))


# List Wardrobe items by category by user
items_by_category_by_user <- plot_data %>% group_by(user, category) %>%
  summarise(total_items = n()) %>%
  arrange(desc(total_items)) %>%
  spread(category, total_items) %>%
  as.data.frame()
write.csv(items_by_category_by_user,"Plots/Z/Z-Items_by_category_by_user.csv", row.names = FALSE)


################################################################################
######### Plot normalised item price distribution by category and user #########
################################################################################

# Select and filter input data
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(user, category, item, price) %>%
  arrange(user, category, desc(price))

# Initiate index with zero as default
plot_data$index <- 0

# Set index per user and category
for (user in unique(plot_data$user)) {
  for (category in unique(plot_data$category)) {
    if (nrow(plot_data[plot_data$user == user & plot_data$category == category,]) > 0) {
      plot_data[plot_data$user == user & plot_data$category == category,]$index <-
        seq.int(nrow(plot_data[plot_data$user == user & plot_data$category == category,]))
    }
  }
}


# Cap price at 150 € to make graphs comparable
plot_data$price[plot_data$price > 150] <- 150

## Plot Diary wears distribution by category plots for all users
for (user in unique(plot_data$user)) {
  p <- plot_data %>%
    filter(!(category %in% c("Accessories", "Other"))) %>%
    setup_price_distribution_user_plot(user = user, ymax = 150, y_break = 50, xmax = 30, x_break = 5, legend = TRUE) +
    facet_grid(rows = vars(category))
  
  filename <- paste("Z-Item_price_distribution_by_user_", user, ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

## Function: Item Diary wears distribution by category
setup_price_distribution_user_plot <- function(plot_data, users, xmax = NA, x_break = 10, ymax = NA, y_break = 100, legend = FALSE) {
  
  # Filter data by user
  if(!is.na(users)) { plot_data <- plot_data %>% filter(user %in% users) }
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$index, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$price, na.rm = TRUE) }
  author_label_x <- xmax
  author_label_y <- ymax
  
  p <-ggplot(
    plot_data,
    aes(x = index, y = price, fill = category)) +
    geom_bar(stat="identity") +
    scale_x_continuous(limits=c(0,xmax), breaks = seq(from = 0, to = xmax, by = x_break)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = y_break)) +
    scale_fill_manual(name = "Category", values = category_colors) +
    labs(x = "Items in decreasing order of price", y = "Item price (capped at 150 €)") +
    ggtitle("Item price distribution by category")
  
  if (!legend){ p <- p + theme(legend.position = "none") }
  
  return(p)
}




########################################
## Wears per Wash by category by user ##
########################################

# Calculate WPW at the item level
WPW <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(user, category, item, wears, washed) %>%
  mutate(wears_per_wash = round(wears / washed, digits = 1))

# Replace infinite WPW with wears for items with no washes
WPW[is.infinite(WPW$wears_per_wash),]$wears_per_wash <- WPW[is.infinite(WPW$wears_per_wash),]$wears

# Summarise WPW data
WPW_cat <- WPW %>%
  group_by(user, category) %>%
  summarise(wears_per_wash = round(mean(wears_per_wash, na.rm = TRUE), digits = 1)) %>%
  as.data.frame() %>%
  spread(category, wears_per_wash, fill = NA) %>%
  as.data.frame() %>%
  arrange(user)

# Write output to CSV file
write.csv(WPW_cat,"Plots/Z/Z-Wears_per_Wash_by_categoty_by_user.csv", row.names = FALSE)



######################################################################
################## Plot CPW and Wears by category ####################
######################################################################

## TOTAL WEARS

# Prepare plot data for plotting Total wears (including estimates of prior use)
plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  mutate(plot_wears = total_wears + wears) %>% 
  select(category, cpw, plot_wears) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

# Plot all categories # CONTINUE HERE: Figure our why the legend is not showing >>>
for (category in category_order) {
  p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = TRUE, xmax = NA, xbreak = 50, ymax = NA, legend = TRUE)
  filename <- paste("Z-CPW_and_Total_wears-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}


## DIARY WEARS

# Prepare plot data for plotting Diary wears (and calculating CPW base only on those)
plot_data <- raw_data %>% filter(!(user %in% excluded_users))

# Create help variable to calculate CPW for items with 0 wears
plot_data$wears_CPW <- plot_data$wears # Copy 
plot_data$wears_CPW[plot_data$wears_CPW == 0] <- 1 # Replace all 0 wears with 1 to in effect set CPW to price for these items
plot_data <- plot_data %>%
  mutate(plot_wears = wears, cpw = price/wears_CPW) %>% 
  select(user, category, cpw, plot_wears, price) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

# Plot all categories
for (category in category_order) {
  if (max(plot_data$plot_wears[category == category]) > 150) { xbreak = 10 } else { xbreak = 5 }
  p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = NA, ymax = NA)
  filename <- paste("Z-CPW_and_Diary_wears-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}


# Explore single users

plot_data %>% group_by(user, category) %>%
  summarise(items = n()) %>% arrange(desc(items)) %>% as.data.frame() %>% head(40)

plot_user <- "Sophy"
category_selected <- "Trousers and jeans"
xbreak = 10
ymax_plot = plot_data %>% filter(category == category_selected) %>% select(cpw) %>% max()
p <- plot_data %>%
#  filter(user  == plot_user) %>%
  filter(plot_wears >= 4) %>%
  setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE,
                           xbreak = xbreak, xmax = 160, ymax = ymax_plot)
p


category_selected <- "Trousers and jeans"
# Calculate category total value and items
category_items <- plot_data %>% filter(category == category_selected) %>% nrow()
category_value <- plot_data %>% filter(category == category_selected) %>%
  summarise(value = sum(price))

# Share of items
plot_data %>% filter(category == category_selected) %>%
  filter(plot_wears > 50) %>% nrow() / category_items

# Share of value
plot_data %>% filter(category == category_selected) %>%
  filter(plot_wears > 50) %>% summarise(value = sum(price)) / category_value




### Plot category CPW progression

# Calculate wear segments

# Prepare plot data for plotting Diary wears (and calculating CPW base only on those)
plot_data <- raw_data

# Create help variable to calculate CPW for items with 0 wears
plot_data$wears_CPW <- plot_data$wears # Copy 
plot_data$wears_CPW[plot_data$wears_CPW == 0] <- 1 # Replace all 0 wears with 1 to in effect set CPW to price for these items
plot_data <- plot_data %>%
  mutate(plot_wears = wears, cpw = price/wears_CPW) %>% 
  select(user, category, cpw, plot_wears, price) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

nr_users <- 29


raw_data %>% filter(date_purchased <= "2021-09-01" | is.na(date_purchased)) %>% nrow()
  

# Set average total number of items during 12 month period
total_items <- round(mean(c(raw_data %>% filter(is.na(date_divested)) %>% nrow(),
                      raw_data %>% filter(date_purchased <= "2021-09-01" | is.na(date_purchased)) %>% nrow())),
                     digits = 0)
# Set average total value of items during 12 month period
total_value <- round(mean(c(raw_data %>% filter(is.na(date_divested)) %>% select(price) %>% sum(na.rm = TRUE),
                            raw_data %>% filter(date_purchased <= "2021-09-01" | is.na(date_purchased)) %>% select(price) %>% sum(na.rm = TRUE))),
                     digits = 0)

plot_table <- plot_data %>%
  #filter(plot_wears == 0) %>%
  #filter(plot_wears >= 4 & plot_wears <= 50) %>%
  filter(plot_wears > 50) %>%
  group_by(category) %>%
  summarise(items = round(n(), digits = 0), wears_mean = round(mean(plot_wears), digits = 0), wears_median = round(median(plot_wears), digits = 0),
            cpw_mean = round(mean(cpw), digits = 2), cpw_median = round(median(cpw), digits = 2),
            value = round(sum(price), digits = 2)) %>%
  select(category, items, value, wears_mean, cpw_mean) %>%
  as.data.frame()

write.csv(plot_table,"Plots/Z/Z-CPW_export.csv", row.names = FALSE)

category <- "Trousers and jeans"
xbreak <-  10
p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = NA, ymax = NA)




# Calculate median purchase price per category as starting point

raw_data %>% select(user, category, item, price, wears) %>%
  group_by(category) %>%
  summarise(median_price = median(price, na.rm = TRUE), average_wears = mean(wears), max_wears = max(wears)) %>%
  as.data.frame()

raw_data %>% select(user, category, item, price, wears) %>% 
#  filter(wears >= 4) %>%
  group_by(category) %>%
  summarise(median = median(wears), mean = mean(wears))
  
  
  filter(category == "Shirts and blouses") %>%
  summarise(wears_quantile = quantile(wears, 0.76))

# List top worn underwear and socks to find hidden item bundles
raw_data %>% select(user, category, item, price, wears) %>%
  filter(category == "Sportswear") %>%
  arrange(desc(wears)) %>%
  as.data.frame() %>% head(30)
  

######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################


## Item age distribution (histograms)

## Item age and Diary wears

user_genders <- user_data %>% select(user, gender)
plot_data <- raw_data %>%
#  filter(user %in% user_genders$user[user_genders$gender == "Male"]) %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  select(category, item, wears, price, date_purchased) %>%
  mutate(wardrobe_age = (as.numeric(as.Date("2022-09-01") - as.Date(date_purchased))) / 365)

# Set negative wardrobe ages to zero
plot_data$wardrobe_age[plot_data$wardrobe_age < 0] <- 0


## Create histogram data manually to include items with 0 years as its own bin

# Set histogram bin limits and labels. There are used for correct plot order also
histogram_age_tiers_bin_max <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
histogram_age_tiers_bin_label <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "Over 10")
plot_data <- plot_data %>%
  select(category, wardrobe_age) %>% 
  mutate(bin = as.factor(case_when(
    wardrobe_age <= histogram_age_tiers_bin_max[1] ~ histogram_age_tiers_bin_label[1],
    wardrobe_age <= histogram_age_tiers_bin_max[2] ~ histogram_age_tiers_bin_label[2],
    wardrobe_age <= histogram_age_tiers_bin_max[3] ~ histogram_age_tiers_bin_label[3],
    wardrobe_age <= histogram_age_tiers_bin_max[4] ~ histogram_age_tiers_bin_label[4],
    wardrobe_age <= histogram_age_tiers_bin_max[5] ~ histogram_age_tiers_bin_label[5],
    wardrobe_age <= histogram_age_tiers_bin_max[6] ~ histogram_age_tiers_bin_label[6],
    wardrobe_age <= histogram_age_tiers_bin_max[7] ~ histogram_age_tiers_bin_label[7],
    wardrobe_age <= histogram_age_tiers_bin_max[8] ~ histogram_age_tiers_bin_label[8],
    wardrobe_age <= histogram_age_tiers_bin_max[9] ~ histogram_age_tiers_bin_label[9],
    wardrobe_age >  histogram_age_tiers_bin_max[10] ~ histogram_age_tiers_bin_label[10]
  ))) %>%
  filter(!is.na(bin))


# Prepare histogram data, including cumulative counts and shares
plot_data <- plot_data %>%
  group_by(category, bin) %>%
  summarise(items = n()) %>% as.data.frame() %>%
  arrange(factor(bin, levels = histogram_age_tiers_bin_label)) %>%
  group_by(category) %>%
  mutate(share = items/sum(items)) %>%
  mutate(cum_items = cumsum(items), cum_share = cumsum(share)) %>%
  as.data.frame()

  plot_data$bin <- factor(plot_data$bin, levels=histogram_age_tiers_bin_label)



# Plot shares for all categories
for (category in category_order) {
  p <- plot_data %>% setup_Diary_age_histogram_plot(categories = category, ymax = NA, ybreak = 0.05, cumulative = FALSE)
  filename <- paste("Z-Diary_wears_and_item_age-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
#  save_to_cloud_Z(filename)
}

# Plot cumulative shares for all categories
for (category in category_order) {
  p <- plot_data %>% setup_Diary_age_histogram_plot(categories = category, ymax = 1, ybreak = 0.05, cumulative = TRUE)
  filename <- paste("Z-Diary_wears_and_item_age-cumulative-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


## Function: Item Diary age histogram by category
setup_Diary_age_histogram_plot <- function(plot_data, categories = NA, ymax = 1, ybreak = 0.1, cumulative = FALSE, legend = TRUE) {
  
  # Filter data by category
  if(!is.na(categories)) { plot_data <- plot_data %>% filter(category %in% categories) }

  if(is.na(ymax)) { ymax <- max(plot_data$share, na.rm = TRUE) }

  
  # Draw plot based on data type
  if(cumulative) {
    p <-ggplot(plot_data,
               aes(x = factor(bin, level = histogram_age_tiers_bin_label), y = cum_share, fill = category)) +
      geom_col() +
      annotate("text", x = histogram_age_tiers_bin_label[length(histogram_age_tiers_bin_label)], y = ymax, label = author_label, color = "gray", hjust = 0)
  }
  else {
    p <-ggplot(plot_data,
               aes(x = factor(bin, level = histogram_age_tiers_bin_label), y = share, fill = category)) +
      geom_col() +
      annotate("text", x = histogram_age_tiers_bin_label[length(histogram_age_tiers_bin_label)], y = ymax, label = author_label, color = "gray", hjust = 1)
  }
  

  # Finish standard plot components
  p <- p +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = ybreak)) +
    scale_fill_manual(name = "Category", values = category_colors[match(categories, category_order)])
  
  # Add title and share percentages to column bases
  if(cumulative) {
    p <- p + labs(x = "Item age (years)", y = "Share of items") +
      ggtitle("Item age tier cumulative distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_age_tiers_bin_label), y = 0, label=scales::percent(cum_share, accuracy = 1L)), color =  "white", label.size = NA)
  }
  else {
    p <- p + labs(x = "Item age (years)", y = "Share if items") +
      ggtitle("Item age tier distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_age_tiers_bin_label), y = share, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA) +
      geom_label(aes(factor(bin, level = histogram_age_tiers_bin_label), y = 0, label=scales::percent(cum_share, accuracy = 1L)), color =  "white", label.size = NA)
  }
    
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
  return(p)
}



## Item age and Diary wears - Individual user plots

user_genders <- user_data %>% select(user, gender)
plot_data <- raw_data %>%
  #  filter(user %in% user_genders$user[user_genders$gender == "Male"]) %>%
  filter(!(user %in% excluded_users)) %>%
  filter(is.na(date_divested)) %>%
  select(user, category, item, wears, price, date_purchased) %>%
  mutate(wardrobe_age = (as.numeric(as.Date("2022-09-01") - as.Date(date_purchased))) / 365)

# Set negative wardrobe ages to zero
plot_data$wardrobe_age[plot_data$wardrobe_age < 0] <- 0


## Create histogram data manually to include items with 0 years as its own bin

# Set histogram bin limits and labels. There are used for correct plot order also
histogram_age_tiers_bin_max <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
histogram_age_tiers_bin_label <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "Over 10")
plot_data <- plot_data %>%
  select(user, category, wardrobe_age) %>% 
  group_by(user) %>%
  mutate(bin = as.factor(case_when(
    wardrobe_age <= histogram_age_tiers_bin_max[1] ~ histogram_age_tiers_bin_label[1],
    wardrobe_age <= histogram_age_tiers_bin_max[2] ~ histogram_age_tiers_bin_label[2],
    wardrobe_age <= histogram_age_tiers_bin_max[3] ~ histogram_age_tiers_bin_label[3],
    wardrobe_age <= histogram_age_tiers_bin_max[4] ~ histogram_age_tiers_bin_label[4],
    wardrobe_age <= histogram_age_tiers_bin_max[5] ~ histogram_age_tiers_bin_label[5],
    wardrobe_age <= histogram_age_tiers_bin_max[6] ~ histogram_age_tiers_bin_label[6],
    wardrobe_age <= histogram_age_tiers_bin_max[7] ~ histogram_age_tiers_bin_label[7],
    wardrobe_age <= histogram_age_tiers_bin_max[8] ~ histogram_age_tiers_bin_label[8],
    wardrobe_age <= histogram_age_tiers_bin_max[9] ~ histogram_age_tiers_bin_label[9],
    wardrobe_age >  histogram_age_tiers_bin_max[10] ~ histogram_age_tiers_bin_label[10]
  ))) %>%
  filter(!is.na(bin))


# Prepare histogram data, including cumulative counts and shares
plot_data <- plot_data %>%
  group_by(user, category, bin) %>%
  summarise(items = n()) %>% as.data.frame() %>%
  arrange(factor(bin, levels = histogram_age_tiers_bin_label)) %>%
  group_by(user, category) %>%
  mutate(share = items/sum(items)) %>%
  mutate(cum_items = cumsum(items), cum_share = cumsum(share)) %>%
  as.data.frame()

plot_data$bin <- factor(plot_data$bin, levels=histogram_age_tiers_bin_label)

# Plot all users
for (active_user in users) {
  user_plot_data <- plot_data %>%
    filter(!(category %in% c("Accessories", "Other"))) %>%
    filter(user == active_user)
  p <- user_plot_data %>% setup_item_age_distribution_user_plot(ymax = max(user_plot_data$items), y_break = max(user_plot_data$items)) +
    facet_grid(rows = vars(category))

  filename <- paste("Z-Item_age_distribution_by_user_", active_user, ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
#  save_to_cloud_Z(filename)
}

## Function: Item age distribution by category
setup_item_age_distribution_user_plot <- function(plot_data, xmax = NA, x_break = 1, ymax = 1.0, y_break = 0.2, legend = FALSE) {
  
  # Filter data by user
#  if(!is.na(users)) { plot_data <- plot_data %>% filter(user %in% users) }
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$index, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$price, na.rm = TRUE) }
  author_label_x <- xmax
  author_label_y <- ymax
  
  p <-ggplot(
    plot_data,
    aes(x = bin, y = items, fill = category)) +
    geom_bar(stat="identity") +
    scale_x_discrete(limits=histogram_age_tiers_bin_label) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq(from = 0, to = ymax, by = y_break)) +
    scale_fill_manual(name = "Category", values = category_colors) +
#    geom_label(aes(factor(bin, level = histogram_age_tiers_bin_label), y = ymax/2, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA) +
    geom_label(aes(factor(bin, level = histogram_age_tiers_bin_label), y = ymax/2, label=items), color =  "white", label.size = NA) +
    labs(x = "Item age (years)", y = "Number of items") +
    ggtitle("Item age distribution by category")
  
  if (!legend){ p <- p + theme(legend.position = "none") }
  
  return(p)
}











## Item age and Diary wears

plot_data <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  select(user, category, item, wears, price, date_purchased) %>%
  mutate(wardrobe_age = (as.numeric(as.Date("2022-09-01") - as.Date(date_purchased))) / 365)

# Set negative wardrobe ages to zero
plot_data$wardrobe_age[plot_data$wardrobe_age < 0] <- 0
# Cap wardrobe age at 15 years (180 months)
plot_data$wardrobe_age[plot_data$wardrobe_age > 15] <- 15

p <- plot_data %>% filter(user == "Robin") %>%
  setup_wears_and_age_plot(categories = "Trousers and jeans", ymax = NA, ybreak = 0.5, xmax = NA, xbreak = 10, trendline = TRUE, log_trans = FALSE)
p

# Plot diary wears by item age for all categories 
for (category in category_order) {
  p <- plot_data %>% setup_wears_and_age_plot(categories = category, ymax = NA, ybreak = 0.5, xmax = NA, xbreak = 10, trendline = TRUE, log_trans = FALSE)
  filename <- paste("Z-Diary_wears_and_item_age-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)



# Function: to setup category point plot y = item age, x = times worn
setup_wears_and_age_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 10, ybreak = 0.5, ybreaks = plot_log_breaks, log_trans=TRUE, avg_lines=TRUE, trendline = TRUE, legend = TRUE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$wears, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$wardrobe_age, na.rm = TRUE) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = wears, y = wardrobe_age, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(breaks = seq.int(from = 0, to = xmax, by = xbreak), limits=c(0,xmax)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Diary wears", y = "Item wardrobe age (years)")
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks) }
  else { p <- p + scale_y_continuous(breaks = seq.int(from = 0, to = ymax, by = ybreak), limits=c(NA,ymax)) }
  
  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  if (avg_lines) { p <- p +
    geom_vline(xintercept = median(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$wears, na.rm=TRUE), y = min(plot_data$wears, na.rm=TRUE)+0.05, label=round(median(plot_data$wears, na.rm=TRUE), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$wardrobe_age, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$wardrobe_age, na.rm=TRUE), label=round(median(plot_data$wardrobe_age, na.rm=TRUE), digits = 0)), color =  "darkgrey")
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
  return(p)
}


p <- plot_data %>% setup_wears_and_price_plot(categories = "T-shirts and tops", ymax = NA, ybreak = 25, xmax = NA, xbreak = 10, trendline = TRUE, log_trans = FALSE)
p

# Function: to setup category point plot y = item price, x = times worn
setup_wears_and_price_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 10, ybreak = 25, ybreaks = plot_log_breaks, log_trans=FALSE, avg_lines=TRUE, trendline = TRUE, legend = TRUE) {
  
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
    labs(x = "Diary wears", y = "Item purchase price")
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks, labels=scales::dollar_format(suffix = "€", prefix = "")) }
  else { p <- p + scale_y_continuous(breaks = seq.int(from = 0, to = ymax, by = ybreak), limits=c(NA,ymax), labels=scales::dollar_format(suffix = "€", prefix = "")) }
  
  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  if (avg_lines) { p <- p +
    geom_vline(xintercept = median(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$wears, na.rm=TRUE), y = min(plot_data$wears, na.rm=TRUE)+0.05, label=round(median(plot_data$wears, na.rm=TRUE), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$price, na.rm=TRUE), label=paste(round(median(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey")
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
}







## Do users wear expensive clothes more often than affordable ones over time?
## Purchase price and diary wears + time in wardrobe

# Create bins for garment age in months
histogram_age_tiers_bin_max <- c(365, 730, 1095, 1095)
histogram_age_tiers_bin_label <- c("< 1 year", "1-2 years", "2-3 years", "3+ years")

plot_data <- raw_data %>% filter(!(user %in% excluded_users)) %>%
  select(user, category, item, wears, price, date_purchased, date_divested) %>%
  mutate(wardrobe_age = as.numeric(as.Date("2022-09-01") - as.Date(date_purchased))) %>%
  mutate(bin = as.factor(case_when(
    wardrobe_age <= histogram_age_tiers_bin_max[1] ~ histogram_age_tiers_bin_label[1],
    wardrobe_age <= histogram_age_tiers_bin_max[2] ~ histogram_age_tiers_bin_label[2],
    wardrobe_age <= histogram_age_tiers_bin_max[3] ~ histogram_age_tiers_bin_label[3],
    wardrobe_age > histogram_age_tiers_bin_max[4] ~ histogram_age_tiers_bin_label[4]
  )))


# Set plot palette
histogram_age_tiers_bin_color_order <<- c("< 1 year", "1-2 years", "2-3 years", "3+ years")
histogram_age_tiers_bin_colors <<- c('#7777FF', '#77FF77', '#FFFF77', '#FF7733')
names(histogram_age_tiers_bin_colors) <- histogram_age_tiers_bin_color_order

p <- plot_data %>% 
  setup_wears_and_price_by_age_plot(categories = "Trousers and jeans", xmax = NA, xbreak=10, ymax = 200, ybreak = 25, log_trans=FALSE, trendline = FALSE)
p

# Function: to setup category point plot y = purchase price, x = times worn
setup_wears_and_price_by_age_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 1, ybreak = 10, ybreaks = plot_log_breaks, log_trans=FALSE, avg_lines=TRUE, trendline = FALSE, legend = TRUE) {
  
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
    aes(x = wears, y = price, colour = bin)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(breaks = seq.int(from = 0, to = xmax, by = xbreak), limits=c(0,xmax)) +
    scale_color_manual(name = "Age", values = histogram_age_tiers_bin_colors) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Diary wears", y = "Purchase price (€)") +
    ggtitle(paste("Item Diary wears and price by wardrobe age - ",categories, sep=""))
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks, labels=scales::dollar_format(suffix = "€", prefix = "")) }
  else { p <- p + scale_y_continuous(breaks = seq.int(from = 0, to = ymax, by = ybreak), limits=c(NA,ymax), labels=scales::dollar_format(suffix = "€", prefix = "")) }
  
  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  if (avg_lines) { p <- p +
    geom_vline(xintercept = median(plot_data$wears, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = median(plot_data$wears, na.rm=TRUE), y = min(plot_data$wears, na.rm=TRUE)+0.05, label=round(median(plot_data$wears, na.rm=TRUE), digits = 0)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$price, na.rm=TRUE), label=paste(round(median(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey")
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.70))
  }
  
  return(p)
}


# Calculate divested items' wardrobe age

plot_data <- raw_data %>% filter(!(user %in% excluded_users)) %>%
  filter(!is.na(date_divested)) %>%
  filter(date_divested > "2021-09-01") %>%
  mutate(wardrobe_age_days = as.numeric(as.Date(date_divested) - as.Date(date_purchased))) %>%
  mutate(wardrobe_age = round(wardrobe_age_days/365, digits = 1)) %>%
  select(user, category, item, price, wears, cpw, wardrobe_age, divestment_way, condition) %>%
  arrange(category)

# Write output to CSV file
write.csv(plot_data,"Plots/Z/Z-Divested_item_wardrobe_age.csv", row.names = FALSE)


plot_data%>% group_by(category) %>%
  summarise(items = n(), avg_wardrobe_age = mean(wardrobe_age, na.rm = TRUE)) %>%
  mutate(avg_wardrobe_age_years = round(avg_wardrobe_age/365, digits = 1))
  

# Plot item age when divested by purchase price for all categories 
for (category in category_order) {
  p <- plot_data %>% setup_divested_item_age_and_price_plot(categories = category, ymax = NA, ybreak = 25, xmax = NA, xbreak = 1, trendline = TRUE, log_trans = FALSE)
  filename <- paste("Z-Divested_item_wardrobe_age_and_price-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/Z/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_Z(filename)
}

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)



# Function: to setup category point plot y = item price, x = item wardrobe age then divested
setup_divested_item_age_and_price_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 0.25, ybreak = 25, ybreaks = plot_log_breaks, log_trans=FALSE, avg_lines=TRUE, trendline = TRUE, legend = TRUE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$wardrobe_age_years, na.rm = TRUE) }
  if(is.na(ymax)) { ymax <- max(plot_data$price, na.rm = TRUE) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = wardrobe_age_years, y = price, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(breaks = seq.int(from = 0, to = xmax, by = xbreak), limits=c(0,xmax)) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Item wardrobe age when divested + mean (years)", y = "Item purchase price")
  
  if (log_trans) { p <- p + scale_y_continuous(trans="log10", limits=c(NA,ymax), breaks=ybreaks, labels=scales::dollar_format(suffix = "€", prefix = "")) }
  else { p <- p + scale_y_continuous(breaks = seq.int(from = 0, to = ymax, by = ybreak), limits=c(NA,ymax), labels=scales::dollar_format(suffix = "€", prefix = "")) }
  
  if (trendline) { p <- p + geom_smooth(method = "lm") }
  
  if (avg_lines) { p <- p +
    geom_vline(xintercept = mean(plot_data$wardrobe_age_years, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$wardrobe_age_years, na.rm=TRUE), y = min(plot_data$wardrobe_age_years, na.rm=TRUE)+0.05, label=round(mean(plot_data$wardrobe_age_years, na.rm=TRUE), digits = 1)), color =  "darkgrey") +
    geom_hline(yintercept = median(plot_data$price, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = median(plot_data$price, na.rm=TRUE), label=paste(round(median(plot_data$price, na.rm=TRUE), digits = 0), "€")), color =  "darkgrey")
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
}









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
repaired_items_by_user <- raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(user) %>% summarise(repaired_items = n(), repaired_value = sum(price, na.rm = TRUE)) %>%
  as.data.frame()
write.csv(repaired_items_by_user,"Plots/Z/Z-Repaired_items_by_user.csv", row.names = FALSE)

# List number of active items and total value by user
wardrobes_by_user <- raw_data %>% filter(!is.na(item)) %>%
  filter(is.na(date_divested)) %>%
  group_by(user) %>% summarise(total_items = n(), total_value = sum(price, na.rm = TRUE)) %>%
  arrange(desc(total_items)) %>%
  as.data.frame()
write.csv(wardrobes_by_user,"Plots/Z/Z-Item_count_and_value_by_user.csv", row.names = FALSE)

# List number of items that need repair by user
items_needing_repair <- raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  filter(is.na(date_divested)) %>%
  group_by(user) %>% summarise(total_items = n(), total_value = sum(price, na.rm = TRUE)) %>%
  as.data.frame()
write.csv(items_needing_repair,"Plots/Z/Z-Items_needing_repair.csv", row.names = FALSE)


# List items by need for repair and actual repairs
item_repair_delta <- raw_data %>%
  filter(is.na(date_divested)) %>%
  filter(needs_repair_start | needs_repair) %>%
  filter(!is.na(needs_repair)) %>%
  select(category, item, needs_repair_start, needs_repair, repaired) %>%
  as.data.frame()
write.csv(item_repair_delta,"Plots/Z/Z-Item_repair_delta.csv", row.names = FALSE)


# List count of items by category for type of repair  for not repairing  
raw_data %>% filter(!is.na(item) & needs_repair > 0) %>%
  as.data.frame() %>%
  group_by(repair_kind, category) %>%
  tally() %>%
  spread(category, n, fill = 0) %>%
  as.data.frame()

str(raw_data)


# List number of repaired items by category
repaired_items_by_category <- raw_data %>% filter(repaired > 0 & !is.na(repaired)) %>%
  group_by(category) %>%
  summarise(repaired_items = n())
write.csv(repaired_items_by_category,"Plots/Z/Z-Repaired_items_by_category.csv", row.names = FALSE)


raw_data %>% group_by(user) %>% 
  summarise(items = n(), repaired = sum(repaired, na.rm = TRUE))




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







#########################################
############ WEARS PER MONTH ############
#########################################



## Months available

# Functions for calculating the difference in months between two dates
monnb <- function(d) {
  lt <- as.POSIXlt(as.Date(d, origin="1900-01-01"))
  lt$year*12 + lt$mon
} 
mondf <- function(d1, d2) { monnb(d2) - monnb(d1) }

# Set diary period starting date to use in filtering
diary_starting_date <- as.Date("2021-09-01")


# Set the total active tracking time in months
months_tracked <- 12

# Set max WPM to every day (avg 30.5 days per month)
WPM_max = 30.5

# Calculate deltas
WPM_delta <-
  raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(!is.na(total_wears)) %>%
  mutate(months_available = mondf(date_purchased, diary_starting_date)) %>%
  filter(months_available > 0) %>%
  select(user, category, item, months_available, wears_real = wears, wears_est = total_wears, date_divested) %>%
  mutate(WPM_est = round(wears_est / months_available, digits = 1),
         WPM_real = round(wears_real / months_tracked, digits = 1),
         WPM_delta = round(WPM_est / WPM_real, digits = 1),
         divested = !is.na(date_divested)) %>%
  select(user, category, item, months_available, wears_real, wears_est, WPM_est, WPM_real, WPM_delta, divested) %>%
  as.data.frame()

# Replace infinite values resulting from zero real uses with WPM for 1 wear (0.1 WPM rounded)
WPM_delta$WPM_delta[is.infinite(WPM_delta$WPM_delta)] <-
  round(WPM_delta$WPM_est[is.infinite(WPM_delta$WPM_delta)] / 0.1, digits = 1)

WPM_delta_cat <- 
  WPM_delta %>%
  group_by(user, category) %>%
  summarise(wears_real = sum(wears_real), est = sum(WPM_est), real = sum(WPM_real)) %>%
  mutate(delta_to_real = round(est/real, digits = 1), delta_to_max = round(est/WPM_max, digits = 1)) %>%
  arrange(desc(delta_to_max)) %>%
  as.data.frame()

# Replace infinite values resulting from zero real uses with WPM for 1 wear (0.1 WPM rounded)
WPM_delta_cat$delta_to_real[is.infinite(WPM_delta_cat$delta_to_real)] <-
  round(WPM_delta_cat$est[is.infinite(WPM_delta_cat$delta_to_real)] / 0.1, digits = 1)


# Calculate real category WPM
WPM_delta %>% group_by(category, user) %>%
  summarise(WPM_real = sum(wears_real) / months_tracked) %>% as.data.frame() %>%
  group_by(category) %>%
  summarise(avg_WPM_real = mean(WPM_real)) %>% as.data.frame()

# Calculate category WPM average delta to real
WPM_delta_cat %>% group_by(category) %>%
  summarise(avg_delta_to_real = mean(delta_to_real, na.rm = TRUE)) %>% as.data.frame()


WPM_delta %>% filter(user == "Szilvia", category == "T-shirts and tops") %>% arrange(desc(WPM_delta))
WPM_delta_cat %>% filter(user == "Szilvia", category == "T-shirts and tops") %>% arrange(desc(delta_to_real))

WPM_delta %>% filter(user == "Florian", category == "T-shirts and tops") %>% arrange(desc(WPM_delta))
WPM_delta_cat %>% filter(user == "Florian", category == "T-shirts and tops") %>% arrange(desc(delta_to_real))


WPM_delta %>% filter(category == "T-shirts and tops") %>% arrange(desc(WPM_delta)) %>% head(15)
WPM_delta_cat %>% filter(category == "T-shirts and tops") %>% arrange(desc(delta_to_real)) %>% head(15)

WPM_delta %>% filter(category == "Shoes and footwear") %>% arrange(desc(WPM_delta)) %>% head(15)
WPM_delta_cat %>% filter(category == "Shoes and footwear") %>% arrange(desc(delta_to_real)) %>% head(15)

# Show top users or categories
WPM_delta %>% 
  filter(!(category %in% c("Underwear and socks", "Nightwear and homewear", "Accessories", "Sportswear", "Other"))) %>%
#  filter(category %in% category_order[8:9]) %>%
  arrange(desc(wears_real)) %>%
  head(15)


# Plot WPM
p <- WPM_delta_cat %>% 
  filter(category %in% "Shoes and footwear") %>%
  setup_WPM_delta_plot_categories(xmax = 5, ymax = 2, wpm_max = 1.0)
p
ggsave(filename = "Plots/Z/WPM-Estimate_vs_max_and_real-example.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("WPM-Estimate_vs_max_and_real-example.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


raw_data %>% select(user, category, item, repaired) %>%
  filter(repaired > 0, user == "Senna")


### Heatmaps

# Remove unfit categories
WPM_delta_hm <- WPM_delta_cat %>%
  filter(!(category %in% c("Underwear and socks", "Accessories", "Other")))


## Heatmap of real WPM by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$real, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=15 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>15 & breaks[-1]<=30.5 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>30.5 & breaks[-1]<=61 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>61), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)

# Build heatmap
hm_data <- WPM_delta_hm %>% 
  select(user, category, real) %>%
  spread("category", real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE)

hm_data %>% heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "Real category WPM (items summed)",
            key.title = FALSE, key.xlab = "Wears Per Month",
            cellnote = hm_data, notecol="black")

# Real WPM with color scaling per row
hm_data <- WPM_delta_hm %>% 
  select(user, category, real) %>%
  spread("category", real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE)

hm_data %>% heatmap.2(scale = "row", trace = "none", density.info = "none",
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "Real WPM during diary period",
            key.title = FALSE, key.xlab = "Category WPM",
            cellnote = hm_data, notecol="black")


## Heatmap of WPM delta to max by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$delta_to_max, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=1 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>1 & breaks[-1]<=1.5 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>1.5 & breaks[-1]<=2 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>2), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)

# Build heatmap
hm_data <- WPM_delta_hm %>% 
  select(user, category, delta_to_max) %>%
  spread("category", delta_to_max) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE)

hm_data %>% heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "User-estimated WPM compared to max (1x)",
            key.title = FALSE, key.xlab = "Multiplier of max",
            cellnote = hm_data, notecol="black")


## Heatmap of WPM delta to real by user and category

# Setup heatmap breaks and color scales
breaks = seq(0, max(WPM_delta_hm$delta_to_real, na.rm = TRUE), length.out=100)
gradient1 = colorpanel( sum( breaks[-1]<=1 ), "green", "white" )
gradient2 = colorpanel( sum( breaks[-1]>1 & breaks[-1]<=3 ), "white", "yellow" )
gradient3 = colorpanel( sum( breaks[-1]>3 & breaks[-1]<=15 ), "yellow", "red" )
gradient4 = colorpanel( sum( breaks[-1]>15), "red", "darkred" )
hm.colors = c(gradient1, gradient2, gradient3, gradient4)


# Build heatmap
hm_data <- WPM_delta_hm %>%
  select(user, category, delta_to_real) %>%
  spread("category", delta_to_real) %>%
  tibble::column_to_rownames(var = "user") %>%
  data.matrix(rownames.force = TRUE)

hm_data %>% heatmap.2(scale = "none", trace = "none", density.info = "none",
            breaks = breaks, col = hm.colors,
            srtCol = 30, cexRow = 0.9, cexCol = 0.9, keysize = 1,
            main = "User-estimated WPM compared to real (1x)",
            key.title = FALSE, key.xlab = "Multiplier of real",
            cellnote = hm_data, notecol="black")







## Have users changed their initial pre-study wear estimates?

# Load raw_data after initial study period in July 2021
load("Data/Threddit-Z-raw_data-2021-07-09.rda")
raw_data_July21 <- raw_data
# Load the latest raw_data
load("Data/Threddit-Z-raw_data.Rda")

str(raw_data_July21)
wears_last <- raw_data %>% select(category, user, item, date_purchased, total_wears = total_wears)
wears_orig <- raw_data_July21 %>% select(category, user, item, date_purchased, total_wears_orig = total_wears)

# List changes made in prior wears estimates 
merge(wears_last, wears_orig, by = c("user", "category", "item")) %>%
  mutate(wears_change = total_wears - total_wears_orig) %>%
  filter(!(wears_change == 0))

wears_last %>% filter(item == "Thrifted black leather oversized jacket")
wears_orig %>% filter(item == "Thrifted black leather oversized jacket")




### Regression models for use

str(raw_data)
model_data <- raw_data %>% 
  filter(!(user %in% excluded_users)) %>%
  mutate(wardrobe_age = (as.numeric(as.Date("2022-09-01") - as.Date(date_purchased))) / 365) %>%
  select(user, category, item, wears, cpw, price, total_wears, date_purchased, date_divested, wardrobe_age)

# Set negative wardrobe ages to zero
model_data$wardrobe_age[model_data$wardrobe_age < 0] <- 0
# Cap wardrobe age at 15 years (180 months)
model_data$wardrobe_age[model_data$wardrobe_age > 15] <- 15


model_data_sample <- model_data %>% filter(category == "Sportswear")
wears_model <- lm(wears ~ price, data = model_data_sample)
summary(wears_model)
summary(wears_model)$coefficient



# New items purchase share totals
total_value <- raw_data %>% summarise(value = sum(price, na.rm = TRUE))
new_value <- raw_data %>% filter(as.Date(date_purchased) > as.Date("2021-09-01")) %>%
  summarise(value = sum(price, na.rm = TRUE))
new_value / total_value
1 / (new_value / total_value)



### DIVESTEMENTS

# Number of divested items
sum(!is.na(raw_data$date_divested))

# List of divested items
raw_data[!is.na(raw_data$date_divested),]

# List divestment way
raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(!is.na(date_divested) & date_divested > "2021-09-01") %>%
  #filter(!is.na(date_divested)) %>%
  group_by(divestment_way) %>%
  summarise(items_divested = n()) %>%
  mutate(share_of_total = round(items_divested / sum(items_divested), digits = 2)) %>%
  arrange(desc(items_divested)) %>%
  as.data.frame()

# List item condition when divested
raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(!is.na(date_divested) & date_divested > "2021-09-01") %>%
  #filter(!is.na(date_divested)) %>%
  group_by(condition) %>%
  summarise(items_divested = n()) %>%
  mutate(share_of_total = round(items_divested / sum(items_divested), digits = 2)) %>%
  arrange(desc(items_divested)) %>%
  as.data.frame()

# List divestments by divestment way and condition
divestments <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  filter(!is.na(date_divested) & date_divested > "2021-09-01") %>%
  #filter(!is.na(date_divested)) %>%
  group_by(divestment_way, condition) %>%
  summarise(items_divested = n()) %>%
  pivot_wider(names_from = divestment_way, values_from = items_divested)
write.csv(divestments,"Plots/Z/Z-Divested_items_by_way.csv", row.names = FALSE)


# List divestments by user and divestment way
divestments_2 <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  # filter(!is.na(date_divested)) %>%
  filter(!is.na(date_divested) & date_divested > "2021-09-01") %>%
  group_by(user, divestment_way) %>%
  summarise(items_divested = n()) %>%
  pivot_wider(names_from = divestment_way, values_from = items_divested)
write.csv(divestments_2,"Plots/Z/Z-Divested_items_by_user.csv", row.names = FALSE)

# List divestments by category and divestment way
divestments_3 <- raw_data %>%
  filter(!(user %in% excluded_users)) %>%
  # filter(!is.na(date_divested)) %>%
  filter(!is.na(date_divested) & date_divested > "2021-09-01") %>%
  group_by(category, divestment_way) %>%
  summarise(items_divested = n()) %>%
  pivot_wider(names_from = divestment_way, values_from = items_divested)
write.csv(divestments_3,"Plots/Z/Z-Divested_items_by_category.csv", row.names = FALSE)


raw_data %>%
  filter(!(user %in% excluded_users)) %>% group_by(category) %>%
  summarise(quantile = quantile(wears, 0.5))



###################################
########## COMBINATORICS ##########
###################################

### Circlize package testing ###
# https://r-graph-gallery.com/228-add-links-between-regions.html

# Load library
library(circlize)

# Create sample wardrobe data
plot_categories <- category_order[1:10]
items_in_categories <- c(8, 2, 10, 5, 10, 20, 12, 5, 15, 25)
data = data.frame(
  category = c(rep(plot_categories[1],items_in_categories[1]),
             rep(plot_categories[2],items_in_categories[2]),
             rep(plot_categories[3],items_in_categories[3]),
             rep(plot_categories[4],items_in_categories[4]),
             rep(plot_categories[5],items_in_categories[5]),
             rep(plot_categories[6],items_in_categories[6]),
             rep(plot_categories[7],items_in_categories[7]),
             rep(plot_categories[8],items_in_categories[8]),
             rep(plot_categories[9],items_in_categories[9]),
             rep(plot_categories[10],items_in_categories[10])),
  item = c(seq(1,items_in_categories[1]),
        seq(1,items_in_categories[2]),
        seq(1,items_in_categories[3]),
        seq(1,items_in_categories[4]),
        seq(1,items_in_categories[5]),
        seq(1,items_in_categories[6]),
        seq(1,items_in_categories[7]),
        seq(1,items_in_categories[8]),
        seq(1,items_in_categories[9]),
        seq(1,items_in_categories[10])), 
  y = runif(sum(items_in_categories))
)

# Order categories
data$category <- factor(data$category, levels = category_order[1:10])


# Set line color and alpha
line_color <- rgb(0.5,0.5,0.5,0.2)

# Initialize the plot.
circos.clear()
par(mar = c(1, 1, 1, 1) ) 
circos.initialize(factors = data$category,
                  xlim = cbind(rep(0.5, 10), items_in_categories+0.5))

# Build the regions of track #1
circos.trackPlotRegion(sectors = data$category, y=data$y , bg.col = category_colors[1:10] , bg.border = NA,
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")

                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         aa = c(1, 0.2)
                         if(theta < 90 || theta > 270)  aa = c(0, 0.5)
                         
                         #plot category labels
                         circos.text(x=mean(xlim), y=1.5, labels=name, facing = dd, cex=0.6,  adj = aa)

                          #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(0,items_in_categories[i],1), 
                                     minor.ticks=1)
                         
                       }
)


# Create sample combinations data
number_of_combinations <- 200 
skewed_distribution <- c(1, 1, 1, 2, 3, 3, 3, 4, 7, 8, 10)
combinations <- data.frame(
  cat_1 = sample(plot_categories, number_of_combinations, replace = TRUE),
  item_1 = sample(skewed_distribution, number_of_combinations, replace = TRUE),
  cat_2 = sample(plot_categories, number_of_combinations, replace = TRUE),
  item_2 = sample(skewed_distribution, number_of_combinations, replace = TRUE)
)


# Subset combinations
plot_combinations <- combinations %>% filter(cat_1 == "Jackets and coats")

# Draw combinations
for(i in 1:nrow(plot_combinations)){
  circos.link(plot_combinations$cat_1[i],
              plot_combinations$item_1[i],
              plot_combinations$cat_2[i],
              plot_combinations$item_2[i],
              h = 1, col = line_color, lwd = 2)
}




# https://r-graph-gallery.com/122-a-circular-plot-with-the-circlize-package.html

### You need several libraries
library(migest)

### Make data
m <- data.frame(order = 1:6,
                country = c("Ausralia", "India", "China", "Japan", "Thailand", "Malaysia"),
                V3 = c(1, 150000, 90000, 180000, 15000, 10000),
                V4 = c(35000, 1, 10000, 12000, 25000, 8000),
                V5 = c(10000, 7000, 1, 40000, 5000, 4000),
                V6 = c(7000, 8000, 175000, 1, 11000, 18000),
                V7 = c(70000, 30000, 22000, 120000, 1, 40000),
                V8 = c(60000, 90000, 110000, 14000, 30000, 1),
                r = c(255,255,255,153,51,51),
                g = c(51, 153, 255, 255, 255, 255),
                b = c(51, 51, 51, 51, 51, 153),
                stringsAsFactors = FALSE)
df1 <- m[, c(1,2, 9:11)]
m <- m[,-(1:2)]/1e04
m <- as.matrix(m[,c(1:6)])
dimnames(m) <- list(orig = df1$country, dest = df1$country)
#Sort order of data.frame and matrix for plotting in circos
df1 <- arrange(df1, order)
df1$country <- factor(df1$country, levels = df1$country)
m <- m[levels(df1$country),levels(df1$country)]


### Define ranges of circos sectors and their colors (both of the sectors and the links)
df1$xmin <- 0
df1$xmax <- rowSums(m) + colSums(m)
n <- nrow(df1)
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)

### Plot sectors (outer part)
par(mar=rep(0,4))
circos.clear()

### Basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 90, gap.degree =4)

### Sector details
circos.initialize(factors = df1$country, xlim = cbind(df1$xmin, df1$xmax))

### Plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$country, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa = c(0, 0.5)
                         
                         #plot country labels
                         circos.text(x=mean(xlim), y=1.7, labels=name, facing = dd, cex=0.6,  adj = aa)
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(from=0,to=floor(df1$xmax)[i],by=5), 
                                     minor.ticks=1)
                       })

### Plot links (inner part)
### Add sum values to df1, marking the x-position of the first links
### out (sum1) and in (sum2). Updated for further links in loop below.
df1$sum1 <- colSums(m)
df1$sum2 <- numeric(n)

### Create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
df2 <- cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
               timevar="dest", time=rownames(m),  v.names = "m")
df2 <- arrange(df2,desc(m))

### Keep only the largest flows to avoid clutter
df2 <- subset(df2, m > quantile(m,0.6))

### Plot links
for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$country)
  j<-match(df2$dest[k],df1$country)
  
  #plot link
  circos.link(sector.index1=df1$country[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$country[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[i])
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}




# Example 2

# Libraries
library(circlize)
library(viridisLite)
library(tibble)
library(patchwork)
library(hrbrthemes)
library(chorddiag)  #devtools::install_github("mattflor/chorddiag")

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)

# short names
colnames(data) <- c("Africa", "East Asia", "Europe", "Latin Ame.",   "North Ame.",   "Oceania", "South Asia", "South East Asia", "Soviet Union", "West.Asia")
rownames(data) <- colnames(data)

# I need a long format
data_long <- data %>%
  rownames_to_column %>%
  gather(key = 'key', value = 'value', -rowname)

# parameters
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

# color palette
mycolor <- viridis(10, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:10)]

# Base plot
chordDiagram(
  x = data_long, 
  grid.col = mycolor,
  transparency = 0.25,
  directional = 1,
  direction.type = c("arrows", "diffHeight"), 
  diffHeight  = -0.04,
  annotationTrack = "grid", 
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow", 
  link.sort = TRUE, 
  link.largest.ontop = TRUE)

# Add text and axis
circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    
    # Add names to the sector. 
    circos.text(
      x = mean(xlim), 
      y = 3.2, 
      labels = sector.index, 
      facing = "bending", 
      cex = 0.8
    )
    
    # Add graduation on axis
    circos.axis(
      h = "top", 
      major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 2, no = 1)), 
      minor.ticks = 1, 
      major.tick.percentage = 0.5,
      labels.niceFacing = FALSE)
  }
)





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
 
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.8, 0.8))
  }
  
  return(p)
}



## Function: Number of wardrobe items vs share of items worn worn_count+ times
setup_share_worn_plot <- function(plot_data, categories, xmax = NA, ymax = 1, trendline = TRUE, worn_count = 1, legend = TRUE) {
  
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
#    p <- p + labs(x = "Amount of items in wardrobe", y = paste("Share of wardrobe items unworn during the diary period", sep = "")) +
#      ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)")
    p <- p + labs(x = "Amount of items in wardrobe", y = paste("Share of wardrobe items worn at least ", worn_count, " times during the diary period", sep = "")) +
      ggtitle("Number of wardrobe items vs share of items worn (each dot is one user's wardrobe)")
    #geom_label(aes(x = 2, y = 0.1 , label=paste("Worn at least\n", worn_count, " times", sep = "")), color =  "darkred", fill = "white")
      
  }
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.15, 0.85))
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
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
  return(p)
}




# Function: to setup category point plot y = purchase price, x = times worn
setup_category_plot <- function(plot_data, categories, xmax = NA, ymax = NA, xbreak = 1, ybreak = 10, ybreaks = plot_log_breaks, log_trans=TRUE, avg_lines=TRUE, trendline = TRUE, legend = TRUE) {
  
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
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
  return(p)
}


# Function: Average item value and number of items by category (across all users)
setup_category_distribution_plot <- function(plot_data, categories, xmax = NA, xbreak = 5, ymax = NA, ybreak = 10, legend = FALSE) {
  
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
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = xbreak)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = ybreak), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Average number of items", y = "Average item value (purchase price)") +
    ggtitle("Average item value and number of items by category (across all users)")
  
  if (!legend){ p <- p + theme(legend.position = "none") }
  
  return(p)
}

# Function: Average item value and number of items by category by user
setup_category_distribution_plot_by_user <- function(plot_data, users, categories, xmax = NA, xbreak = 5, ymax = NA, ybreak = 10, legend = FALSE) {
  
  # Filter data by category
  plot_data <- plot_data %>% filter(category %in% categories, user %in% users)
  
  # Set author label coordinates (upper right corner)
  if(is.na(xmax)) { xmax <- max(plot_data$items) }
  if(is.na(ymax)) { ymax <- max(plot_data$average_value) }
  
  author_label_x <- xmax
  author_label_y <- ymax
  
  # Set plot_size
  plot_size <- 0.5
  
  # Set up plot
  p <- ggplot(
    plot_data, 
    aes(x = items, y = average_value, colour = category)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    geom_label_repel(aes(label=category)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = xbreak)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = ybreak), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Total number of items", y = "Average item value (purchase price)") +
    ggtitle("Average item value and number of items by category")
  
  if (!legend){ p <- p + theme(legend.position = "none") }
  
  return(p)
}


# Function: Wardrobe size and value by user (all categories)
setup_user_distribution_plot <- function(plot_data, xmax = NA, ymax = NA, xbreak = 20, ybreak = 500, legend = TRUE) {
  
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
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = xbreak)) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = ybreak), labels=scales::dollar_format(suffix = "€", prefix = "")) +
    #scale_color_manual(name = "Category", values = category_colors[match(categories, category_order)]) +
    scale_color_manual(name = "Users", values = gender_colors) +
    scale_alpha(range = c(0.5, 1.0)) +
    scale_size(range = c(2, 3)) +
    guides(alpha = FALSE, size = FALSE) +
    labs(x = "Number of items", y = "Value of items (at purchase price)") +
    ggtitle("Wardrobe size and value by user (all categories)")
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.9, 0.1))
  }
  
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
  
  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
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
setup_CPW_and_Wears_plot <- function(plot_data, categories, plot_total_wears = TRUE, xmax = NA, xbreak = 10, ymax = NA, ybreak = 25, log_trans = TRUE, trendline = FALSE, avg_lines = TRUE, guides = TRUE, legend = TRUE) {
  
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

  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.85, 0.85))
  }
  
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
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.05, label=scales::percent(cum_share, accuracy = 1L)), color =  "white", label.size = NA)
  }
  else {
    p <- p + labs(x = "Diary wears", y = "Share of items in Diary wears tier") +
      ggtitle("Item Diary wears tier distribution (all users)") +
      geom_label(aes(factor(bin, level = histogram_wear_tiers_bin_label), y = 0.01, label=scales::percent(share, accuracy = 1L)), color =  "white", label.size = NA)
  }

  if (!legend){
    p <- p + theme(legend.position = "none")
  } else {
    p <- p + theme(legend.position = c(0.5, 0.9))
  }
  
  return(p)
}







