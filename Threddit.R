### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to process and plot Threddit data
### Input: Excel file with specific structure and formatting


#################################################################################################
###################################### SET UP ENVIRONMENT #######################################
###################################### SET UP ENVIRONMENT #######################################
###################################### SET UP ENVIRONMENT #######################################
#################################################################################################

# Remove all objects from workspace
rm(list = ls())

# Load required packages
library(readxl)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(gganimate)
library(ggimage)
library(transformr)
library(gifski)
library(imager)
library(roll)
library(googlesheets4)

# Source required files
source("01-Read_and_preprocess_data.R")
source("02-Calculate_active_use_data.R")
source("03-Calculate_plot_data.R")
source("04-Google_Drive_access.R")

## Set up static variables

# Set category order (DEPENDENCY)
category_order = c("Jackets and hoodies", "Blazers and vests", "Knits",
                   "Shirts", "T-shirts and tanks", "Pants", "Shorts", "Belts",
                   "Socks", "Shoes", "Underwear shirts", "Underwear boxers",
                   "Sportswear")


#################################################################################################
################################ READ AND PROCESS STANDARD DATA #################################
#################################################################################################

# This section calls the functions to read, clean and transform use data,
# as well as calulate the data for the standard plots

## Read and clean master raw data

# Excel
#raw_data_file <- "Threddit.xlsx"
#masterdata <- read_data(raw_data_file)

# Google Sheets
gs4_auth() # Authenticate Tidyverse packages to Google Drive
masterdata <- read_data_GD(get_Google_sheet_ID()) # Read master data from Google Drive


## Transform data
plotuse <- transform_data(masterdata) %>% # 1) Transform raw data into tidy data
calculate_active_use_data() %>% # 2) Calculate cumulative use data
calculate_total_use_data() %>% # 3) Calculate total use data, including divested items 
calculate_plot_data() # 4) Calculate plot data for the standard plots


## Save master plotting data 'plotuse' to file to avoid repetitive reprocessing

# Save tidy data data.frame to file for easier retrieval
save(plotuse,file="Data/Threddit-plotuse-2020-11-29.Rda")

# Load data from file
load("Data/Threddit-plotuse-2020-11-29.Rda")



#################################################################################################
################################ SET UP PLOTTING ENVIRONMENT ####################################
#################################################################################################

### This section sets up the plotting environment

# Set gray and white theme
theme_set(theme_gray())

# Set plot palette for 13 categories
category_colors <- c('#960001', '#FC3334', '#FF9A02', '#FFDB05', '#4CDA00', '#00B0F0',
                      '#0070C0', '#002060', '#A860E9', '#7030A0', '#A5A5A5', '#7B7B7B', '#444444')

# Set color names by category name for consistent category colors in plots
names(category_colors) <- levels(plotuse$category)

# Set category photos
category_photos <- data.frame(
  "category" = levels(plotuse$category),
  "photo" = c('Photos/Category-Jackets_and_hoodies.png',
              'Photos/Category-Blazers_and_vests.png',
              'Photos/Category-Knits.png',
              'Photos/Category-Shirts.png',
              'Photos/Category-T-shirts_and_tanks.png',
              'Photos/Category-Pants.png',
              'Photos/Category-Shorts.png',
              'Photos/Category-Belts.png',
              'Photos/Category-Socks.png',
              'Photos/Category-Shoes.png',
              'Photos/Category-Underwear_shirts.png',
              'Photos/Category-Underwear_boxers.png',
              'Photos/Category-Sportswear.png'))

# Set rolling average window size to 30 days
rolling_average_window <- 30

# Set author label to add in the upper right corner of plots
author_label <- "hoverfalt.github.io"




##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

plot_data %>% filter(category == "Shirts" & date == max(plot_data$date))

p <- plot_data %>% filter(category == "Shirts" & date == max(plot_data$date)) %>%
  ggplot(aes(x = use_per_month, y = cost_per_use)) +
  geom_image(aes(image = photo), size = 0.08) +
  annotate("text", x = max(use_per_month), y = max(cost_per_use), label = "Olof")

dev.off()  

p <- plot_data %>% setup_category_plot_image(categories = "Shirts", xmax = NA, ymax = NA, log_trans=TRUE)





### CATEGORY PLOT - COST PER USE vs CUMULATIVE USE - ANIMATED ###

# DEVELOPMENT DEVELOPMENT #

animation <- plot_data %>% filter(category == 'Shoes') %>%
  ggplot(aes(x = cumuse, y = cost_per_use)) +
  geom_image(aes(image = photo), size = 0.08) +
  scale_x_continuous(limits=c(0,320)) +
  labs(x = "Cumulative times used", y = "Cost per use (€)") +
  scale_y_continuous(trans="log10", limits=c(NA,16)) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')

p <- p + transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')

animate(p, height = 1000, width = 1000, nframes = 100, fps = 24, end_pause = 72) # Frames = states + end pause
animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72) # Frames = states + end pause
anim_save("Plots/Category-Shoes-Cumulative_use-animation.gif")




################################################################################################
##################################### END OF DEVELOPMENT #######################################
################################################################################################













#################################################################################################
######################################## PORTFOLIO PLOTS ########################################
######################################## PORTFOLIO PLOTS ########################################
######################################## PORTFOLIO PLOTS ########################################
#################################################################################################

### Calculate active inventory item count and value by category
inventory <- calculate_portfolio_plot_data(plotuse)


### STANDARD PORTFOLIO PLOTS #################################################################################################

# Active inventory item count by category
p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_item_count_plot()
ggsave(filename = "Website/Plots/Portfolio-Inventory-Item_count.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')
dev.off()

# Active inventory value by category (line plot)
p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_by_category_plot()
ggsave(filename = "Website/Plots/Portfolio-Inventory-Value_by_category.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')
dev.off()

# Active inventory value by category (stacked area plot)
p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_stacked_plot()
ggsave(filename = "Website/Plots/Portfolio-Inventory-Value_stacked.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')
dev.off()




### DAILY COST WITH ROLLING AVERAGE ##########################################################################################

# Calculate daily cost and rolling average (Sportswear excluded by default, can be overridden)
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")

# Portfolio plot: daily cost and rolling average
p <- setup_daily_cost_plot(daily_cost, ymax = 40, seasons = TRUE)
ggsave(filename = "Website/Plots/Portfolio-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()



## Category daily cost with rolling average ##

# Plot: Jackets and hoodies
daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Jackets and hoodies")
p <- setup_daily_cost_plot(daily_cost_category, ymax = 8)
ggsave(filename = "Website/Plots/Category-Jackets_and_hoodies-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()




# Add rest of categories (TODO)

# Plot: Shirts
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shirts")
p <- setup_daily_cost_plot(daily_cost, ymax = 20)
ggsave(filename = "Plots/Category-Shirts-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Plot: Shoes
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shoes")
p <- setup_daily_cost_plot(daily_cost, ymax = 20)
ggsave(filename = "Plots/Category-Shoes-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Plot: Underwear including socks
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = c("Underwear shirts","Underwear boxers","Socks"))
p <- setup_daily_cost_plot(daily_cost, ymax = 10)
ggsave(filename = "Plots/Category-Underwear_and_socks-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()





## Animations

# Prepare data for animated daily cost plotting (HEAVY COMPUTING)
daily_cost_anim <- calculate_daily_cost_anim(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")

# Subset data to exclude NAs in 30-day rolling average (this is to avoid transition_time faiure in animation)
daily_cost_anim_plot <- daily_cost_anim %>% filter(day >= daterange[rolling_average_window] & day <= daterange[length(daterange)-rolling_average_window])

# Reduce frames by removing every second day (note: not date!)
daily_cost_anim_plot_reduced <- daily_cost_anim_plot[daily_cost_anim_plot$day %in% unique(daily_cost_anim_plot$day)[c(TRUE, FALSE)],]

# Set up animation, animate, and save (HEAVY COMPUTING)
setup_daily_cost_animation(daily_cost_anim_plot_reduced, ymax = 40) %>%
animate(height = 1000, width = 1000, nframes = length(unique(daily_cost_anim_plot_reduced$day)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Portfolio-Daily_cost-animation.gif")



## Animation: Jackets and hoodies
# Prepare data (HEAVY COMPUTING) and subset. Set up animation, animate, and save (HEAVY COMPUTING).
daily_cost_anim <- calculate_daily_cost_anim(plotuse, rolling_average_window, categories_include = c("Jackets and hoodies"))
daily_cost_anim_plot <- daily_cost_anim %>% filter(day >= daterange[rolling_average_window] & day <= daterange[length(daterange)-rolling_average_window])
daily_cost_anim_plot_reduced <- daily_cost_anim_plot[daily_cost_anim_plot$day %in% unique(daily_cost_anim_plot$day)[c(TRUE, FALSE)],]
setup_daily_cost_animation(daily_cost_anim_plot_reduced, ymax = 20) %>%
animate(height = 1000, width = 1000, nframes = length(unique(daily_cost_anim_plot$day)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Jackets_and_hoodies-Daily_cost-animation.gif")







### AVERAGE DAILY/YEARLY COST vs CATEGORY USE ################################################################################

# Calculate complete portfolio data
usetodate <- calculate_complete_portfolio_plot_data(plotuse)
# Add category photos to data frame
usetodate_anim <- merge(usetodate, category_photos, by.x = "category")


# Image plot: Average DAILY cost vs category use
p <- usetodate_anim %>% setup_daily_cost_and_category_use_plot(animate = FALSE)
ggsave(filename = "Website/Plots/Portfolio-Daily_cost_and_Category_use.png", p, width = 12, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Image plot: Average YEARLY cost vs category use
p <- usetodate_anim %>% setup_yearly_cost_and_category_use_plot(animate = FALSE)
ggsave(filename = "Website/Plots/Portfolio-Yearly_cost_and_Category_use.png", p, width = 12, height = 10, dpi = 300, units = "in", device=png())
dev.off()



## Animations

# Reduce frames by removing every second date
usetodate_anim_reduced <- usetodate_anim[usetodate_anim$date %in% unique(usetodate_anim$date)[c(TRUE, FALSE)],] # Reduce frames

# Animation: Average DAILY cost vs category use (HEAVY COMPUTING)
animation <- usetodate_anim_reduced %>% setup_daily_cost_and_category_use_plot(ymax = 15, animate = TRUE)
animate(animation, height = 1000, width = 1150, nframes = length(unique(usetodate_anim_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Portfolio-Daily_cost_and_Category_use-animation.gif")

# Animation: Average YEARLY cost vs category use (HEAVY COMPUTING)
animation <- usetodate_anim_reduced %>% setup_yearly_cost_and_category_use_plot(ymax = 1000, animate = TRUE)
animate(animation, height = 1000, width = 1150, nframes = length(unique(usetodate_anim_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Portfolio-Yearly_cost_and_Category_use-animation.gif")











################################################################################################
######################################## CATEGORY PLOTS ########################################
######################################## CATEGORY PLOTS ########################################
######################################## CATEGORY PLOTS ########################################
################################################################################################

### The following functions are used to set up standard category plots
### Image plot: setup_category_plot_image(plot_data, cat, xmax, ymax, log_trans=TRUE, animated=FALSE)
### Point plot: setup_category_plot_point(plot_data, cat, xmax, ymax, log_trans=TRUE, avg_lines=TRUE)


### SET UP CATEGORY PLOTS MASTER DATA ###

# All following plotting sections rely on this data to be prepared first.

# Create category plots master data and group by date to calculate averages
plot_data <- plotuse %>% group_by(category, date)

# Calculate use weighted average use per month and average cost per use for divested items
avg_merge_divested <- plot_data %>% filter(active == FALSE &  days_active >= 30) %>%
    select(category, item, date, use_per_month, cost_per_use, days_active, cumuse) %>%
    mutate(avg_use_per_month_divested = sum(cumuse) / sum(days_active) * 30.5, avg_cost_per_use_divested = sum(cost_per_use * cumuse) / sum(cumuse)) %>%
    select(-use_per_month, -cost_per_use, -days_active, -cumuse)
plot_data <- merge(plot_data, avg_merge_divested, all = TRUE)
rm(avg_merge_divested)
plot_data <- plot_data %>% ungroup()

# Create data set (plot_data_reduced) suitable for animation
# Reduce dates (frames) to half for animation: remove every second date (c(T,F) replicates automatically)
# Animation used transition_state(), which avoids rendering multiple dates in one frame, but also
# causes the number of frames to determine the dates to be included. Thus the number of frames needs to
# match the amount of dates to be animated. Reducing the amount of dates (frames) is the best way.

#length(unique(plot_data$date)) # Current length
plot_data_reduced <- plot_data[plot_data$date %in% unique(plot_data$date)[c(TRUE, FALSE)],]
#length(unique(plot_data_reduced$date)) # New length confirmed





### STANDARD CATEGORY IMAGE PLOTS ###

# Create and save image plots for all categories
for (i in category_order){
  p <- plot_data %>% setup_category_plot_image(categories = c(i), xmax = NA, ymax = NA, log_trans=TRUE)
  ggsave(filename = paste("Website/Plots/Category-", gsub(" ", "_", i), ".png", sep=""),
         p, width = 10, height = 10, dpi = 300, units = "in", device=png())
  dev.off()
}


## STANDARD CATEGORY IMAGE PLOTS ANIMATED ##

# Jackets and hoodies
plot_data_reduced %>% setup_category_plot_image("Jackets and hoodies", xmax = 20, ymax = 16, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Jackets_and_hoodies-animation.gif")

# Blazers and vests
plot_data_reduced %>% setup_category_plot_image("Blazers and vests", xmax = 1.5, ymax = 64, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Blazers_and_vests-animation.gif")

# Knits
plot_data_reduced %>% setup_category_plot_image("Knits", xmax = 1.0, ymax = 32, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Knits-animation.gif")

# Shirts
plot_data_reduced %>% setup_category_plot_image("Shirts", xmax = 3, ymax = 20, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Shirts-animation.gif")

# T-shirts and tanks
plot_data_reduced %>% setup_category_plot_image("T-shirts and tanks", xmax = 3, ymax = 16, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-T-shirts_and_tanks-animation.gif")

# Pants
plot_data_reduced %>% setup_category_plot_image("Pants", xmax = 20, ymax = 32, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Pants-animation.gif")

# Shorts
plot_data_reduced %>% setup_category_plot_image("Shorts", xmax = 8, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Shorts-animation.gif")

# Belts
plot_data_reduced %>% setup_category_plot_image("Belts", xmax = 9, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Belts-animation.gif")

# Socks
plot_data_reduced %>% setup_category_plot_image("Socks", xmax = 3, ymax = 10, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Socks-animation.gif")

# Shoes
plot_data_reduced %>% setup_category_plot_image("Shoes", xmax = 9, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Shoes-animation.gif")

# Underwear shirts
plot_data_reduced %>% setup_category_plot_image("Underwear shirts", xmax = 3, ymax = 20, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Underwear_shirts-animation.gif")

# Underwear boxers
plot_data_reduced %>% setup_category_plot_image("Underwear boxers", xmax = 3.5, ymax = 20, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Underwear_boxers-animation.gif")

# Sportswear
plot_data_reduced %>% setup_category_plot_image("Sportswear", xmax = 6, ymax = 50, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Website/Plots/Category-Sportswear-animation.gif")






### CATEGORY PLOT - COST PER USE vs CUMULATIVE USE ###

## Set guides as global variable to be used in plot functions
guides_prices <- c(5, 10, 20, 50, 100, 200, 400, 800)


# Jackets and hoodies
p <- plot_data %>% setup_category_cumulative_plot_image("Jackets and hoodies", xmax = 400, ymax = 10, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Jackets_and_hoodies-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Blazers and vests
p <- plot_data %>% setup_category_cumulative_plot_image("Blazers and vests", xmax = 60, ymax = 50, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Blazers_and_vests-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Knits
p <- plot_data %>% setup_category_cumulative_plot_image("Knits", xmax = 30, ymax = 50, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Knits-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Shirts
p <- plot_data %>% setup_category_cumulative_plot_image("Shirts", xmax = 60, ymax = 45, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Shirts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# T-shirts and tanks
p <- plot_data %>% setup_category_cumulative_plot_image("T-shirts and tanks", xmax = 110, ymax = 20, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-T-shirts_and_tanks-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Pants
p <- plot_data %>% setup_category_cumulative_plot_image("Pants", xmax = 120, ymax = 35, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Pants-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Shorts
p <- plot_data %>% setup_category_cumulative_plot_image("Shorts", xmax = 160, ymax = 100, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Shorts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Belts
p <- plot_data %>% setup_category_cumulative_plot_image("Belts", xmax = 220, ymax = 100, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Belts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Socks
p <- plot_data %>% setup_category_cumulative_plot_image("Socks", xmax = 50, ymax = 10, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Socks-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Shoes
p <- plot_data %>% setup_category_cumulative_plot_image("Shoes", xmax = 340, ymax = 30, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Shoes-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Underwear shirts
p <- plot_data %>% setup_category_cumulative_plot_image("Underwear shirts", xmax = 45, ymax = 42, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Underwear_shirts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Underwear boxers
p <- plot_data %>% setup_category_cumulative_plot_image("Underwear boxers", xmax = 35, ymax = 16, log_trans=TRUE, trails=TRUE, guides=TRUE)
ggsave(filename = "Website/Plots/Category-Underwear_boxers-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())
dev.off()

# Sportswear (this plot does not make sense for Sportswear)






### CATEGORY PLOT - TIMES USED ###

# Create and save image plots for all categories
for (i in category_order){
  p <- setup_category_times_used_plot(plot_data, categories = c(i), animate = FALSE)
  ggsave(filename = paste("Website/Plots/Category-", gsub(" ", "_", i), "-Times_used.png", sep=""),
         p, width = 10, height = 10, dpi = 300, units = "in", device=png())
  dev.off()
}


# ANIMATED PLOTS #

# Reduce dates (frames) to half for animation: remove every second date (c(T,F) replicates automatically)
# Animation used transition_state(), which avoids rendering multiple dates in one frame, but also
# causes the number of frames to determine the date to be included. Thus the number of frames needs to
# match the amount of dates to be animated. Reducing the amount of dates (frames) is the best way.

# Reduce frames
plot_data_reduced <- plot_data[plot_data$date %in% unique(plot_data$date)[c(TRUE, FALSE)],]

# Create and save animations for all categories (HEAVY COMPUTING ~ 12 x 4-8 min = 1h)
for (i in category_order){
  setup_category_times_used_plot(plot_data_reduced, categories = c(i), animate = TRUE) %>%
    animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72) # Frames = states + end pause
  anim_save(paste("Website/Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""))
}
















####################################################################################################
########################################### Unused plots ###########################################
####################################################################################################


## Plot total active inventory value

# List item values from master data
itemvalues <- masterdata %>% select(Item, Price, Category) %>% rename(item = Item, price = Price, category = Category)

# Calculate toal active inventory (this cannot be included in inventory as it is portfolio-level data)
inventory_value_total <- plotuse %>%
  filter(category != "Sportswear") %>% # Exclude Sportswear
  filter(active == TRUE) %>%
  merge(itemvalues, all = TRUE) %>%
  group_by(date) %>%
  summarise(inventoryvalue = sum(price)) 

# Plot: total active inventory value
p <- inventory_value_total %>% 
  ggplot( aes(x = date, y = inventoryvalue)) + geom_line() +
  scale_color_manual(name = "Category", values = category_colors) +
  labs(x = "Date", y = "Active inventory value at purchase price (€)")
ggsave(filename = "Plots/Portfolio-Inventory-Value_total.png", p, width = 10, height = 10, dpi = 300, units = "in", device='png')
dev.off()






### STANDARD CATEGORY POINT PLOTS ###

# Shirts, Pants, Shoes
p <- plot_data %>% setup_category_plot_point(categories = c("Shirts", "Pants", "Shoes"), xmax = 4, ymax = 5, log_trans=FALSE)
ggsave(filename = "Plots/Category-Shirts_Pants_Shoes-point.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# All items
p <- plot_data %>% setup_category_plot_point(categories = names(category_colors), xmax = 8, ymax = 64, log_trans=TRUE, avg_lines=FALSE)
ggsave(filename = "Plots/Category-All-point.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

dev.off()


### STANDARD CATEGORY POINT PLOTS ANIMATED ###

# Shirts, Pants, Shoes
plot_data_reduced %>% setup_category_plot_point(c("Shirts", "Pants", "Shoes"), xmax = 4, ymax = 5, log_trans=TRUE, animate=TRUE) %>%
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Shirts_Pants_Shoes-point-animation.gif")

# Underwear shirts, Underwear boxers, Socks
plot_data_reduced %>% setup_category_plot_point(c("Underwear shirts", "Underwear boxers", "Socks"), xmax = 4, ymax = 3, log_trans=TRUE, animate=TRUE) %>%
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Shirts_Underwear_and_Socks-point-animation.gif")






## AVERAGE DAILY COST vs CATEGORY USE
## Point animation
animation <- ggplot(usetodate, 
                    aes(x = category_use, y = daily_cost, colour = category)) +
  geom_point(show.legend = TRUE, aes(size = category_value, group = date)) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(trans="log10", limits=c(NA,NA)) +
  scale_color_manual(name = "Category", values = category_colors) +
  scale_size(range = c(1, 10)) +
  guides(size = FALSE) +
  labs(x = "Category daily use", y = "Average daily cost (all items)") +
  transition_states(date, state_length = 1, transition_length = 0) +
  labs(title = "Date: {closest_state}") + ease_aes('linear')
animate(animation, height = 1000, width = 1200, nframes = length(unique(usetodate$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Daily_cost_and_Category_use-point.gif")

## Point animation
animation <- ggplot(usetodate, 
                    aes(x = category_use, y = yearly_cost, colour = category)) +
  geom_point(show.legend = TRUE, aes(size = category_value)) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(limits=c(NA,NA)) +
  scale_color_manual(name = "Category", values = category_colors) +
  scale_size(range = c(1, 10)) +
  guides(size = FALSE) +
  labs(x = "Category daily use", y = "Average yearly cost of use (category)") +
  transition_states(date, state_length = 1, transition_length = 0) +
  labs(title = "Date: {closest_state}") + ease_aes('linear')
animate(animation, height = 1000, width = 1200, nframes = length(unique(usetodate$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Yearly_cost_and_Category_use-point.gif")












################################################################################################
########################################### Notebook ###########################################
################################################################################################

# Add trails to animaton 
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}") +
    shadow_mark(alpha = 0.1, size = 0.5)
animate(animation, height = 800, width = 1000, nframes = 120, fps = 10)

# View follow
view_follow(fixed_y = TRUE)


# animation package sources:
# https://towardsdatascience.com/animating-your-data-visualizations-like-a-boss-using-r-f94ae20843e3
# https://github.com/isaacfab/tinker/blob/master/animate_with_r/good_bad_examples.R

# R color map: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# dplyr cheat sheet
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

# GGANIMATE: HOW TO CREATE PLOTS WITH BEAUTIFUL ANIMATION IN R
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/


#start_time <- Sys.time()
#end_time <- Sys.time()
#print(end_time - start_time)
#rm(start_time)
#rm(end_time)







###############################################
### OVERLAY FITBIT STEP DATA  WITH ITEM USE ###
###############################################

### Fetch step data ###

# Load required packages
library(fitbitr)

# Source required files
source("Fitbit-API-Key.R")

# Set Fitbit key and secret
FITBIT_KEY    <- get_fitbit_key()
FITBIT_SECRET <- get_fitbit_secret()
FITBIT_CALLBACK <- "http://localhost:1410/" 

# Authenticate and get token
token <- fitbitr::oauth_token()

# Set date of latest data
date <- "2020-04-25"

# Get daily step data for entire item data period and remove duplicates
steps_2020 <- get_activity_time_series(token, "steps", date=date, period="1y")
steps_2019 <- get_activity_time_series(token, "steps", date="2019-12-31", period="1y")
steps_2018 <- get_activity_time_series(token, "steps", date="2018-12-31", period="1y")
steps <- rbind(steps_2020, rbind(steps_2018, steps_2019))
steps <- steps[!duplicated(steps$dateTime),]

# Remove temporary variables
rm(steps_2020)
rm(steps_2019)
rm(steps_2018)

# Convert variables to correct type and arrange by date
steps <- steps %>%
  mutate(date = as.Date(dateTime, "%Y-%m-%d")) %>%
  mutate(steps = as.numeric(value)) %>%
  select(-dateTime, -value) %>%
  arrange(date)
# Alt: mutate(date = as.POSIXct(strptime(steps$dateTime, "%Y-%m-%d"))) %>%

# Save steps data.frame to file for easier retrieval
save(steps,file="Data/Threddit-steps-2020-04-26.Rda")

# Load data from file
load("Data/Threddit-steps-2020-04-26.Rda")

# Plot steps
ggplot2::ggplot(steps, aes(x=date, y=steps)) + geom_col()



### Add step data to items ###

rm(shoe_use)

# Create shoe use data set
shoe_use <- plotuse %>% filter(category == "Shoes") %>%
  select(item, date, used, cumuse, days_active, active, photo)

# Find days with multiple items used, minimum 1
multiple_use <- shoe_use %>% group_by(date) %>% summarise(count = sum(used))
multiple_use$count[multiple_use$count == 0] <- 1

# Allocate daily steps to item(s) used on corresponding dates
shoe_use <- merge(shoe_use, steps)
shoe_use <- merge(shoe_use, multiple_use)
shoe_use <- shoe_use %>% mutate(steps = steps * used / count) %>% select(-count)

# Remove temporary variable
rm(multiple_use)

# Calculate cumulative steps
shoe_use <- shoe_use %>%
  group_by(item) %>%
  mutate(cumsteps = cumsum(steps)) %>%
  ungroup()

# Initiate variable cumsteps_init 
shoe_use <- shoe_use %>%
  mutate(cumsteps_init = as.double(0))

# Find shoes used prior to data collection started 2018-01-01
steps_init <- shoe_use %>%
  ungroup() %>%
  filter(date == "2018-01-01" & cumuse > 0) %>%
  select(item, cumuse, steps)

# Calculate initial cumulative use for applicable items
for (item in steps_init$item){
  shoe_use$cumsteps_init[shoe_use$item == item] <-
    round(mean(shoe_use$steps[shoe_use$item == item & shoe_use$steps > 0]) * steps_init$cumuse[steps_init$item == item], digits=0)
} 

# Remove temporary variable
rm(steps_init)

# Correct cumulative steps by initial cumulative steps
shoe_use <- shoe_use %>%
  mutate(cumsteps = cumsteps + cumsteps_init) %>%
  select(-cumsteps_init)

str(shoe_use)


### Plot item step data ###

# Print total steps by item
shoe_use %>% group_by(item) %>%
  filter(cumsteps == max(cumsteps) & date == max(date)) %>%
  arrange(desc(cumsteps))

# Plot total steps by item
shoe_use %>% group_by(item) %>%
  filter(cumsteps == max(cumsteps) & date == max(date)) %>%
  arrange(desc(cumsteps)) %>%
  ggplot(aes(x=item, y=cumsteps)) + geom_col() + coord_flip()



