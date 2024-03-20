### Threddit.R - Olof Hoverfält - 2018-2024 - hoverfalt.github.io

# Functions to prepare and plot multi-user data

#################################################################################################
###################################### SET UP ENRIVONENT ########################################
#################################################################################################

# Clear workspace
rm(list = ls())

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
author_label <<- "wardrobediary.io"

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

# Function to upload plot file to Google Cloud Storage
save_to_cloud_O <- function(file_name) {
  
  md5_local <- openssl::base64_encode(digest(paste("Plots/O/", file_name, sep=""), file=TRUE, algo="md5", serialize=FALSE, raw=TRUE))
  md5_cloud <- httr::GET(paste(firebase_img_path_plots, file_name, sep="")) %>% content() %>% { .[["md5Hash"]][1] }
  if (!identical(md5_cloud, md5_local)) {
    gcs_upload(paste("Plots/O/", file_name, sep=""), name=file_name)
    print(paste(file_name, "uploaded"))
  } else {
    print(paste(file_name, "not uploaded, identical file exists"))
  }
}



#################################################################################################
######################################### READ DATA #############################################
#################################################################################################

# Read Google Sheets wardrobe and use data
data_file = get_Google_sheet_ID_O()
raw_data <- read_sheet(data_file, sheet='Use data filtered - Machine readable')

temp <- raw_data
raw_data <- temp

# Pre-process and clean data
raw_data <- raw_data %>% as.data.frame() %>% filter(!is.na(category) & !is.na(item))
raw_data <- raw_data %>% mutate_at(c("wears"), ~replace(., is.na(.), 0)) %>% rename(date_purchased = listed_date)
raw_data <- raw_data %>% mutate(price = as.numeric(price, na.rm = TRUE))

# Turn factor variables into factors
raw_data$user <- factor(raw_data$user, levels = unique(raw_data$user))
raw_data$category <- factor(raw_data$category, levels = category_order)
str(raw_data)

# Read Google Sheets user data
data_file = get_Google_sheet_ID_O2()
user_data <- read_sheet(data_file, sheet='User profiles')
user_data <- user_data %>% as.data.frame() %>% filter(!(user == "END OF LIST"))
save(user_data,file="Data/Threddit-O-user_data.Rda")
load("Data/Threddit-O-user_data.Rda")


# Normalise price and cpw to EUR
exchange_rates <- user_data %>% select(user, exchange_rate)
raw_data <- merge(raw_data, exchange_rates, by="user") %>%
  mutate(price = round(price * exchange_rate, digits = 2), cpw = round(cpw * exchange_rate, digits = 2)) %>%
  select(-exchange_rate)

# Test result
raw_data %>% filter(user == "Kristin-1501") %>% select(user, category, item, price, wears)
length(unique(raw_data$user))

# Save raw_data data.frame to file for easier retrieval
save(raw_data, file="Data/Threddit-O-raw_data.Rda")
# Load data from file
load("Data/Threddit-O-raw_data.Rda")



#################################################################################################
######################################## SET UP PLOTS ###########################################
#################################################################################################


# Count number of users
length(unique(raw_data$user))
length(users_exhaustive)
length(users_full_year)

# Count number of items
nrow(raw_data)

## Average wardrobe size and value by user
# Create plot data
plot_data <- raw_data %>%
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(user) %>%
  summarise(items = n(), share_worn = sum(worn)/n(), value = sum(price, na.rm = TRUE)) %>%
  as.data.frame()

# Merge use and user data to distinguish gender
total_data <- merge(plot_data, user_data, by = c("user"))

total_data %>% filter(user %in% users_exhaustive) %>% select(user) %>% unique() %>% nrow()

# Plot average wardrobe size 
p <- total_data %>%
  filter(user %in% users_exhaustive) %>% # Include exhaustive users only
  filter(!(gender == "Other")) %>% # Protect few users
  setup_user_distribution_plot(xmax = NA, ymax = NA, ybreak = 1000)
ggsave(filename = "Plots/O/O-Average_wardrobe_size_and_value_by_user.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Average_wardrobe_size_and_value_by_user.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


count(user_data[user_data$gender == "Male",]) # Males
count(user_data[user_data$gender == "Female",]) # Females
count(user_data[user_data$gender == "Female",]) / count(user_data) # Share female

sum(plot_data$items) # Total number of items tracked
sum(plot_data$value) # Total value of items tracked

max(total_data$items) # Largest wardrobe in terms of item count
max(total_data$value) # Largest wardrobe in terms of item value


# Secondhand 
raw_data %>% group_by(user) %>% 
  summarise(items = n(), value = round(sum(price, na.rm = TRUE), digits = 0), secondhand = sum(secondhand), share = round(sum(secondhand) / n(), digits = 2)) %>%
  as.data.frame() %>%
  arrange(desc(share))

# Share of users that have at least one second hand item
str(raw_data)
raw_data %>% filter(!is.na(date_divested)) %>% count()
raw_data %>% filter(secondhand) %>% count()
raw_data %>% group_by(user) %>% 
  summarise(items = n(), secondhand = sum(secondhand), share = sum(secondhand) / n())



## Average wardrobe size and value by category

plot_data <- raw_data %>%
  filter(!is.na(category)) %>% # Exclude empty categories
  filter(user %in% users_exhaustive) %>% # Include exhaustive users only
  filter(is.na(date_divested)) %>%
  mutate(worn = as.logical(wears)) %>%
  rowwise() %>%
  group_by(category) %>%
  summarise(items = n(), share_worn = round(sum(worn)/n(), digits = 2), value = sum(price, na.rm = TRUE)) %>%
  mutate(average_items = round(items/length(users_exhaustive), digits = 2), average_value = round(value / items, digits = 2)) %>%
  as.data.frame()

p <- plot_data %>% setup_category_distribution_plot(categories = category_order, xmax = 30, ymax = 100, legend  = FALSE)
ggsave(filename = "Plots/O/O-Average_wardrobe_size_and_value_by_category.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Average_wardrobe_size_and_value_by_category.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)






# Compare average purchase price of new and secondhand by user
temp <- raw_data %>%
  filter(!(category %in% c("Accessories", "Other"))) %>%
  filter(!is.na(price)) %>% filter(price > 0) %>%
  group_by(user, category, secondhand) %>%
  summarise(avg_price = round(mean(price, na.rm = TRUE), digits = 2)) %>%
  spread(secondhand, avg_price, drop = TRUE) %>%
  select(user, category, new = 'FALSE', used = 'TRUE') %>%
  as.data.frame() %>%
  filter(!is.na(new)) %>% filter(!is.na(used)) %>% filter(used > 0) %>%
  mutate(gap = round(used / new, digits = 2))

temp %>% arrange(desc(gap)) %>% head(20)
temp %>% arrange(desc(gap)) %>% tail(20)

# Calculate average gap by category (collapse user dimension)
temp %>% group_by(category) %>%
  summarise(users = n(), avg_gap = round(mean(gap, na.rm = TRUE), digits = 2)) %>%
  as.data.frame()




# Create help variable to calculate CPW for items with 0 wears
plot_data$wears_CPW <- plot_data$wears # Copy 
plot_data$wears_CPW[plot_data$wears_CPW == 0] <- 1 # Replace all 0 wears with 1 to in effect set CPW to price for these items
plot_data <- plot_data %>%
  mutate(plot_wears = wears, cpw = price/wears_CPW) %>% 
  select(user, category, cpw, plot_wears, price, secondhand) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

category_filter <- "Dresses and jumpsuits"
plot_data %>% filter(category == category_filter) %>% nrow()
xbreak <-  10
p <- plot_data %>%
  #filter(secondhand) %>%
  #  filter(plot_wears <= 4) %>%
  #  filter(plot_wears > 4 & plot_wears <= 50) %>%
  #  filter(plot_wears > 50) %>%
  setup_CPW_and_Wears_plot(categories = category_filter, plot_total_wears = FALSE, xbreak = xbreak, xmax = 180, ymin = 0.1, ymax = 200)
ggsave(filename = "Plots/O/O-CPW_and_wears-Trousers_and_jeans-RWT.png", p, width = 9, height = 7, dpi = 150, units = "in")

# Calculate share of items and value for given wear segment
total_items <- plot_data %>%
  group_by(category) %>%
  summarise(items = round(n(), digits = 0), value = round(sum(price, na.rm = TRUE), digits = 2)) %>%
  as.data.frame()

segment_items <- plot_data %>%
  filter(plot_wears < 5) %>%
  group_by(category) %>%
  summarise(items = round(n(), digits = 0), value = round(sum(price, na.rm = TRUE), digits = 2)) %>%
  as.data.frame() 

summary <- merge(total_items, segment_items, by = "category") %>%
  mutate(share_items = round(items.y / items.x, digits = 2),
         share_value = round(value.y / value.x, digits = 2)) %>%
  select(category, share_items, share_value)




# Plot category wears distribution (histogram)
wears_distribution <- raw_data %>%
  select(category, wears)

p <- wears_distribution %>% setup_Diary_wears_distribution_plot(categories = "Trousers and jeans", ymax = 200, y_break = 25, xmax = NA, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/O/O-Wears_distribution-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")




# Compare secondhand and virgin
plot_data %>% group_by(category, secondhand) %>%
  summarise(items = n(), avg_wears = mean(plot_wears), avg_price = mean(price), avg_cpw = mean(cpw)) %>%
  as.data.frame()

# Compare secondhand and virgin: average wears
temp <- raw_data_joint %>%
  filter(!(category %in% c("Accessories", "Other"))) %>%
  filter(!is.na(price)) %>% filter(price > 0) %>%
  group_by(user, category, secondhand) %>%
  summarise(avg_wears = round(mean(wears, na.rm = TRUE), digits = 2)) %>%
  spread(secondhand, avg_wears, drop = TRUE) %>%
  select(user, category, new = 'FALSE', used = 'TRUE') %>%
  as.data.frame() %>%
  filter(!is.na(new)) %>% filter(!is.na(used)) %>% filter(used > 0) %>%
  mutate(gap = round(used / new, digits = 2))

temp %>% filter(is.finite(gap)) %>% arrange(desc(gap)) %>% head(20)
temp %>% filter(is.finite(gap)) %>% arrange(desc(gap)) %>% tail(20)
temp %>% filter(is.finite(gap)) %>% filter(category == "Jackets and coats") %>% arrange(desc(gap))

# Calculate average gap by category (collapse user dimension)
temp %>%filter(is.finite(gap)) %>%
  group_by(category) %>%
  summarise(users = n(), avg_gap = round(mean(gap, na.rm = TRUE), digits = 2)) %>%
  as.data.frame()





# Compare average purchase price of new and secondhand by user
temp <- raw_data %>%
  filter(!(category %in% c("Accessories", "Other"))) %>%
  filter(!is.na(price)) %>% filter(price > 0) %>%
  group_by(user, category, secondhand) %>%
  summarise(avg_price = round(mean(price, na.rm = TRUE), digits = 2)) %>%
  spread(secondhand, avg_price, drop = TRUE) %>%
  select(user, category, new = 'FALSE', used = 'TRUE') %>%
  as.data.frame() %>%
  filter(!is.na(new)) %>% filter(!is.na(used)) %>% filter(used > 0) %>%
  mutate(gap = round(used / new, digits = 2))




# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)

plot_data <- raw_data # No excludes, i.e. include all items for price distribution
str(raw_data)

## Item price distribution by category

p <- plot_data %>% setup_price_distribution_plot(categories = c("Jackets and coats"), xmax = 500, ymax = 225, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 4)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Jackets_and_coats.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Blazers and vests"), xmax = 500, ymax = 150, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 4)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Blazers_and_vests.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Jumpers and hoodies"), xmax = 500, ymax = 350, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Cardigans and knits"), xmax = 500, ymax = 425, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Cardigans_and_knits.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Shirts and blouses"), xmax = 500, ymax = 750, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 16)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Shirts_and_blouses.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("T-shirts and tops"), xmax = 500, ymax = 2000, binwidth = 20, x_break = 20, y_break = 50, repel_gap = 12)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-T-shirts_and_tops.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Dresses and jumpsuits"), xmax = 500, ymax = 450, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Shorts and skirts"), xmax = 500, ymax = 450, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Shorts_and_skirts.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Trousers and jeans"), xmax = 500, ymax = 525, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Trousers_and_jeans.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Shoes and footwear"), xmax = 500, ymax = 350, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Shoes_and_footwear.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Underwear and socks"), xmax = 500, ymax = 2800, binwidth = 20, x_break = 20, y_break = 100, repel_gap = 6)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Underwear_and_socks.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Nightwear and homewear"), xmax = 500, ymax = 525, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Nightwear_and_homewear.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Accessories"), xmax = 500, ymax = 625, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 4)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Accessories.png")

p <- plot_data %>% setup_price_distribution_plot(categories = c("Sportswear"), xmax = 500, ymax = 825, binwidth = 20, x_break = 20, y_break = 25, repel_gap = 14)
ggsave(filename = "Plots/O/O-Item_price_distribution_by_category-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Item_price_distribution_by_category-Sportswear.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)


###############################################
## Plot Diary wears distribution by category ##
###############################################

plot_data <- raw_data %>%
  filter(user %in% users_full_year) %>%
  select(category, wears)

#Share of items with 120+ wears
cat = "Jackets and coats"
plot_data %>% filter(category == cat) %>% filter(wears > 120) %>% nrow() / nrow(plot_data %>% filter(category == cat))

# List 0-5-wear category size
plot_data %>% filter(wears <= 5) %>% group_by(category) %>% summarise(items = n())

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jackets and coats", ymax = 250, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Jackets_and_coats.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Blazers and vests", ymax = 200, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Blazers_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Blazers_and_vests.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Jumpers and hoodies", ymax = 200, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Cardigans and knits", ymax = 430, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 3)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Cardigans_and_knits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shirts and blouses", ymax = 675, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Shirts_and_blouses.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "T-shirts and tops", ymax = 1025, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-T-shirts_and_tops.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Dresses and jumpsuits", ymax = 675, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shorts and skirts", ymax = 425, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 1)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Shorts_and_skirts.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Trousers and jeans", ymax = 500, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Trousers_and_jeans.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Shoes and footwear", ymax = 450, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Shoes_and_footwear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Underwear and socks", ymax = 775, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Underwear_and_socks.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Nightwear and homewear", ymax = 200, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Nightwear_and_homewear.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Accessories", ymax = 400, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 0)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Accessories.png")

p <- plot_data %>% setup_Diary_wears_distribution_plot(categories = "Sportswear", ymax = 550, y_break = 25, xmax = 120, binwidth = 5, x_break = 5, repel_gap = 2)
ggsave(filename = "Plots/O/O-Diary_wears_distribution-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_O("O-Diary_wears_distribution-Sportswear.png")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)




######################################################################
################## Plot CPW and Wears by category ####################
######################################################################


# Prepare plot data for plotting Diary wears (and calculating CPW base only on those)
plot_data <- raw_data %>% filter(user %in% users_full_year)

# Create help variable to calculate CPW for items with 0 wears
plot_data$wears_CPW <- plot_data$wears # Copy 
plot_data$wears_CPW[plot_data$wears_CPW == 0] <- 1 # Replace all 0 wears with 1 to in effect set CPW to price for these items
plot_data <- plot_data %>%
  mutate(plot_wears = wears, cpw = price/wears_CPW) %>% 
  select(user, category, item, cpw, plot_wears, price) %>%
  filter(cpw > 0) # Remove items with purchase price 0 to avoid stretching logarithmic scale

# Plot all categories - full wear range
xbreak <- 10 
for (category in category_order) {
  #if (max(plot_data$plot_wears[category == category]) > 150) { xbreak = 10 } else { xbreak = 5 }
  p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = 120, ymax = 400, ymin = 0.1)
  filename <- paste("O-CPW_and_Diary_wears-", gsub(" ", "_", category), ".png", sep = "")
  ggsave(paste("Plots/O/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_O(filename)
}

# Plot all categories - 0-4 wears
plot_data_segment <- plot_data %>% filter(plot_wears <= 4)
xbreak <- 10 
for (category in category_order) {
  p <- plot_data_segment %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = 120, ymax = 400, ymin = 0.1)
  filename <- paste("O-CPW_and_Diary_wears-", gsub(" ", "_", category), "-0-4.png", sep = "")
  ggsave(paste("Plots/O/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_O(filename)
}

# Plot all categories - 5-50 wears
plot_data_segment <- plot_data %>% filter(plot_wears >= 5 & plot_wears <= 50)
xbreak <- 10 
for (category in category_order) {
  p <- plot_data_segment %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = 120, ymax = 400, ymin = 0.1)
  filename <- paste("O-CPW_and_Diary_wears-", gsub(" ", "_", category), "-5-50.png", sep = "")
  ggsave(paste("Plots/O/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_O(filename)
}

# Plot all categories - 51+ wears
plot_data_segment <- plot_data %>% filter(plot_wears >= 51)
xbreak <- 10 
for (category in category_order) {
  p <- plot_data_segment %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = xbreak, xmax = 120, ymax = 400, ymin = 0.1)
  filename <- paste("O-CPW_and_Diary_wears-", gsub(" ", "_", category), "-51_and_over.png", sep = "")
  ggsave(paste("Plots/O/", filename, sep = ""), p, width = 9, height = 7, dpi = 150, units = "in")
  save_to_cloud_O(filename)
}




# Explore single category
category <- "Trousers and jeans"
category <- "Jackets and coats"
p <- plot_data %>% setup_CPW_and_Wears_plot(categories = category, plot_total_wears = FALSE, xbreak = 25, xmax = NA, ymax = NA)

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
category_selected <- "Jackets and coats"
category_selected <- "Shoes and footwear"
category_selected <- "T-shirts and tops"

# Calculate category total value and items
category_items <- plot_data %>% filter(category == category_selected) %>% nrow()
category_value <- plot_data %>% filter(category == category_selected) %>% summarise(value = sum(price))

# Share of items 0-4
plot_data %>% filter(category == category_selected) %>% filter(plot_wears <= 4) %>% nrow() / category_items
# Share of value 0-4
plot_data %>% filter(category == category_selected) %>% filter(plot_wears <= 4) %>% summarise(value = sum(price)) / category_value

# Share of items 5-50
plot_data %>% filter(category == category_selected) %>% filter(plot_wears >= 5 & plot_wears <= 50) %>% nrow() / category_items
# Share of value 5-50
plot_data %>% filter(category == category_selected) %>% filter(plot_wears >= 5 & plot_wears <= 50) %>% summarise(value = sum(price)) / category_value

# Share of items 51+
plot_data %>% filter(category == category_selected) %>% filter(plot_wears >= 51) %>% nrow() / category_items
# Share of value 5-50
plot_data %>% filter(category == category_selected) %>% filter(plot_wears >= 51) %>% summarise(value = sum(price)) / category_value



# Explore individual brand
str(raw_data)
raw_data[grepl("houdini", raw_data$item, ignore.case = TRUE),] %>% nrow()
raw_data[grepl("houdini", raw_data$item, ignore.case = TRUE),] %>% select(user, category, item, price, wears, secondhand)

str(plot_data)
plot_data[grepl("houdini", plot_data$item, ignore.case = TRUE),] %>% nrow()
plot_data[grepl("houdini", plot_data$item, ignore.case = TRUE),]












## Number of wardrobe items and share of items worn

plot_data <- raw_data %>%
  mutate(worn = as.logical(wears)) %>%
  group_by(user, category) %>%
  summarise(items = n(), share_worn = sum(worn)/n())

p <- plot_data %>% setup_share_worn_plot(categories = c("Jackets and coats"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Jackets_and_coats.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Jackets_and_coats.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Blazers and vests"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Blazer_and_vests.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Blazer_and_vests.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Jumpers and hoodies"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Jumpers_and_hoodies.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Jumpers_and_hoodies.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Cardigans and knits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Cardigans_and_knits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Cardigans_and_knits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shirts and blouses"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Shirts_and_blouses.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Shirts_and_blouses.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("T-shirts and tops"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-T-shirts_and_tops.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-T-shirts_and_tops.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Dresses and jumpsuits"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Dresses_and_jumpsuits.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shorts and skirts"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Shorts_and_skirts.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Shorts_and_skirts.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Trousers and jeans"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Trousers_and_jeans.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Trousers_and_jeans.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Shoes and footwear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Shoes_and_footwear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Shoes_and_footwear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Underwear and socks"), xmax = 80)
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Underwear_and_socks.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Underwear_and_socks.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Nightwear and homewear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Nightwear_and_homewear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Nightwear_and_homewear.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Accessories"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Accessories.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Accessories.png")

p <- plot_data %>% setup_share_worn_plot(categories = c("Sportswear"))
p <- p + geom_smooth(method = "lm")
ggsave(filename = "Plots/O/O-Wardrobe_items_and_share_worn-Sportswear.png", p, width = 9, height = 7, dpi = 150, units = "in")
save_to_cloud_Z("O-Wardrobe_items_and_share_worn-Sportswear.png")




######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################
######################################## DEVELOPMENT WIP ###########################################



## Do users wear expensive clothes more often than affordable ones?
## Purchase price and diary wears 

p <- raw_data %>% filter(wears > 0 & price > 0) %>%
#  filter(user == "Sophy") %>%
  filter(category == "Shoes and footwear") %>%
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
    geom_hline(yintercept = mean(plot_data$share_worn, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = 0, y = mean(plot_data$share_worn, na.rm=TRUE), label=paste("Mean\n", round(mean(plot_data$share_worn*100, na.rm=TRUE), digits = 0), "%")), color =  "darkgrey", fill = "white") +
    geom_vline(xintercept = mean(plot_data$items, na.rm=TRUE), color="darkgrey", linetype="dashed") +
    geom_label(aes(x = mean(plot_data$items, na.rm=TRUE), y = 0, label=paste("Mean\n", round(mean(plot_data$items, na.rm=TRUE), digits = 0))), color =  "darkgrey", fill = "white") +
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
#    geom_vline(xintercept = mean(plot_data$average_items), color="darkgrey", linetype="dashed") +
#    geom_label(aes(x = mean(plot_data$average_items), y = min(plot_data$average_value), label=round(mean(plot_data$average_items), digits = 0)), color =  "darkgrey") +
#    geom_hline(yintercept = mean(plot_data$average_value), color="darkgrey", linetype="dashed") +
#    geom_label(aes(x = 0, y = mean(plot_data$average_value), label=paste(round(mean(plot_data$average_value), digits = 0), "kr")), color =  "darkgrey") +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    geom_label_repel(aes(label=category)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = 2)) +
#    scale_y_continuous(limits=c(NA,ymax), breaks = seq.int(from = 0, to = ymax, by = 100), labels=scales::dollar_format(suffix = " kr", prefix = "")) +
    scale_y_continuous(limits=c(NA,ymax), breaks = seq.int(from = 0, to = ymax, by = 10), labels=scales::dollar_format(suffix = " €", prefix = "")) +
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
#    geom_label(aes(x = 0, y = mean(plot_data$value), label = paste(round(mean(plot_data$value), digits = 0), "kr")), color =  "darkgrey") +
    geom_label(aes(x = 0, y = mean(plot_data$value), label = paste(round(mean(plot_data$value), digits = 0), "€")), color =  "darkgrey") +
    geom_point(show.legend = TRUE, aes(alpha = plot_size, size = plot_size)) +
    scale_x_continuous(limits=c(0,xmax), breaks = seq.int(from = 0, to = xmax, by = 20)) +
#    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = 10000), labels=scales::dollar_format(suffix = " kr", prefix = "")) +
    scale_y_continuous(limits=c(0,ymax), breaks = seq.int(from = 0, to = ymax, by = 1000), labels=scales::dollar_format(suffix = " €", prefix = "")) +
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

