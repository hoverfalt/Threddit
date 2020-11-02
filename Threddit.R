### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to process and plot Threddit data
### Input: Excel file with specific structure and formatting


#################################################################################################
###################################### SET UP ENVIRONMENT #######################################
#################################################################################################

# Remove all objects from workspace
rm(list = ls())

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(gganimate)
library(transformr)
library(gifski)
library(ggimage)
library(lubridate)
library(roll)

# Not actively used packages
library(htmlwidgets)

# Source required files
source("01-Read_and_preprocess_data.R")
source("02-Calculate_active_use_data.R")
source("03-Calculate_plot_data.R")

## Set up static variables

# Set raw data file name
raw_data_file <- "Threddit.xlsx"

# Set the column number where the data starts (DEPENDENCY)
date_column_number <- 12

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

# Read and clean master raw data
masterdata <- read_data(raw_data_file)

# Transform raw data into tidy data
itemuse <- transform_data(masterdata)

# Calculate cumulative use data
cumulativeuse <- calculate_active_use_data(itemuse)

# Calculate total use data, including divested items 
totaluse <- calculate_total_use_data(cumulativeuse)

# Calculate plot data for the standard plots
plotuse <- calculate_plot_data(totaluse)


### Save master plotting data 'plotuse' to file to avoid repetitive reprocessing

# Save tidy data data.frame to file for easier retrieval
save(plotuse,file="Data/Threddit-plotuse-2020-10-25.Rda")

# Load data from file
load("Data/Threddit-plotuse-2020-10-25.Rda")



#################################################################################################
################################ SET UP PLOTTING ENVIRONMENT ####################################
#################################################################################################

### This section sets up the plotting environment and functions 

# Set gray and white theme
theme_set(theme_gray())

# Set plot palette for 13 categories
category_colors <- c('#960001', '#FC3334', '#FF9A02', '#FFDB05', '#4CDA00', '#00B0F0',
                      '#0070C0', '#002060', '#A860E9', '#7030A0', '#A5A5A5', '#7B7B7B', '#444444')

# Set color names by category name for consistent category colors in plots
names(category_colors) <- levels(plotuse$category)




#################################################################################################
######################################## PORTFOLIO PLOTS ########################################
#################################################################################################

### Calculate active inventory item count and value by category
inventory <- calculate_portfolio_plot_data(plotuse)


### STANDARD PORTFOLIO PLOTS ###

# Plot active inventory item count by category
p <- inventory %>% 
    filter(category != "Sportswear") %>% # Exclude Sportswear
    ggplot( aes(x = date, y = itemcount, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory (number of items)")
# Save plot to file
ggsave(filename = "Plots/Portfolio-Inventory-Item_count.png", p, width = 300, height = 250, dpi = 300, units = "mm", device='png')


# Plot active inventory value by category (line plot)
p <- inventory %>% 
    filter(category != "Sportswear") %>% # Exclude Sportswear
    ggplot( aes(x = date, y = categoryvalue, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
# Save plot to file
ggsave(filename = "Plots/Portfolio-Inventory-Value_by_category.png", p, width = 300, height = 250, dpi = 300, units = "mm", device='png')


# Plot active inventory value by category (stacked area plot)
p <- inventory %>% 
    filter(category != "Sportswear") %>% # Exclude Sportswear
    ggplot( aes(x = date, y = categoryvalue, fill = category)) + geom_area() +
    scale_fill_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
# Save plot to file
ggsave(filename = "Plots/Portfolio-Inventory-Value_stacked.png", p, width = 300, height = 250, dpi = 300, units = "mm", device='png')



### Plot total active inventory value

# List item values from master data
itemvalues <- masterdata %>% select(Item, Price, Category) %>% rename(item = Item, price = Price, category = Category)

# Calculate toal active inventory (this cannot be included in inventory as it is portfolio-level data)
inventory_value_total <- plotuse %>%
    filter(category != "Sportswear") %>% # Exclude Sportswear
    filter(active == TRUE) %>%
    merge(itemvalues, all = TRUE) %>%
    group_by(date) %>%
    summarise(inventoryvalue = sum(price)) 

# Plot total active inventory value
p <- inventory_value_total %>% 
    ggplot( aes(x = date, y = inventoryvalue)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
# Save plot to file
ggsave(filename = "Plots/Portfolio-Inventory-Value_total.png", p, width = 300, height = 250, dpi = 300, units = "mm", device='png')


# Plot 30-day rolling average category use
p <- inventory %>% 
    ggplot( aes(x = date, y = rolling_category_use, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "30-day rolling average of category use")
p


# Plot monthly inventory turnaround
p <- ggplot(inventory, aes(x = date, y = category_turnaround, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Monthly inventory turnaround")
p



### Daily cost with rolling average ###

# Set rolling average window size to 30 days
rolling_average_window <- 30

# Calculate daily cost and rolling average (Sportswear excluded by default, can be overridden)
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")

# Plot daily cost and rolling average
p <- setup_daily_cost_plot(daily_cost, ymax = 40)

# Save plot to file
ggsave(filename = "Plots/Portfolio-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Close graphics device
dev.off()



# Category plots
unique(plotuse$category)

# Plot: Shirts
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shirts")
p <- setup_daily_cost_plot(daily_cost, ymax = 20)
ggsave(filename = "Plots/Category-Daily_cost-Shirts.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Plot: Shoes
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shoes")
p <- setup_daily_cost_plot(daily_cost, ymax = 20)
ggsave(filename = "Plots/Category-Daily_cost-Shirts.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Plot: Underwear
daily_cost <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = c("Underwear shirts","Underwear boxers","Socks"))
p <- setup_daily_cost_plot(daily_cost, ymax = 10)
ggsave(filename = "Plots/Category-Daily_cost-Underwear.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

dev.off()




### ANIMATED PORTFOLIo PLOTS ###

# Set rolling average window size to 30 days
rolling_average_window <- 30


## Prepare data for animated daily cost plotting (heavy computing)
daily_cost_anim <- calculate_daily_cost_anim(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")

# Save daily_cost_anim data frame to file for easier retrieval
save(daily_cost_anim,file="Data/Threddit-daily_cost_anim-2020-10-25.Rda")
# Load data from file
load("Data/Threddit-daily_cost_anim-2020-10-25.Rda")


## Build animated daily cost plot

# Subset data to exclude NAs in 30-day rolling average (this is to avoid transition_time faiure in animation)
daily_cost_anim_plot <- daily_cost_anim %>% filter(day >= daterange[rolling_average_window] & day <= daterange[length(daterange)-rolling_average_window])

# Set up animation
animation <-
  ggplot(daily_cost_anim_plot, aes(x = date, y = daily_cost, color = still_active)) +
  geom_point(size=1.5) +
  scale_color_manual(breaks = c(TRUE, FALSE), values=c("mediumseagreen", "indianred1")) +
  geom_line(data = na.omit(daily_cost_anim_plot), aes(x = date, y = average_daily_cost), color='steelblue', size=1.5) +
  scale_y_continuous(limits=c(0,40)) + # Set fixed Y (daily cost) limit at 50 to avoid plot scale from jumping around
  labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "Still active") +
  transition_time(day) + labs(title = "Date: {frame_time}") + ease_aes('linear')

# Animate and save
animate(animation, height = 1000, width = 1000, nframes = 500, fps = 24,end_pause = 72)
anim_save("Plots/Portfolio-Daily_cost-animation.gif")



## Category plot: Shirts

daily_cost_anim_temp <- calculate_daily_cost_anim(plotuse, rolling_average_window, categories_include = "Shirts")

daily_cost_anim_temp_plot <- daily_cost_anim_temp %>% filter(day >= daterange[rolling_average_window] & day <= daterange[length(daterange)-rolling_average_window])

animation <-
  ggplot(daily_cost_anim_temp_plot, aes(x = date, y = daily_cost, color = still_active)) +
  geom_point(size=1.5) +
  scale_color_manual(breaks = c(TRUE, FALSE), values=c("mediumseagreen", "indianred1")) +
  geom_line(data = na.omit(daily_cost_anim_temp_plot), aes(x = date, y = average_daily_cost), color='steelblue', size=1.5) +
  scale_y_continuous(limits=c(0,20)) + # Set fixed Y (daily cost) limit at 50 to avoid plot scale from jumping around
  labs(x = "Date", y = "Daily cost and 30-day rolling average (shifted to midpoint of sample)", color = "Still active") +
  transition_time(day) + labs(title = "Date: {frame_time}") + ease_aes('linear')

animate(animation, height = 1000, width = 1000, nframes = 500, fps = 24,end_pause = 72)
anim_save("Plots/Category-Daily_cost-Shirts-animation.gif")






####################################################################
### Y: Average daily cost (all items), X: Category use (to date) ###
####################################################################

### Calculate complete portfolio data
usetodate <- calculate_complete_portfolio_plot_data(plotuse)


# Plot Y: Average daily cost (all items, active and divested), X: Category use (to date)
p <- ggplot(
    usetodate, 
    aes(x = category_use, y = daily_cost, colour = category)) +
    geom_point(show.legend = TRUE, aes(size = category_value)) +
    scale_x_continuous(limits=c(NA,NA)) +
    scale_y_continuous(trans="log10", limits=c(NA,NA)) +
    scale_color_manual(name = "Category", values = category_colors) +
    scale_size(range = c(1, 10)) +
    guides(size = FALSE) +
    labs(x = "Category daily use", y = "Average daily cost (all items)") +
    ease_aes('linear')

# Animate and save plot
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 1000, width = 1200, nframes = 500, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Daily_cost_and_Category_use-point.gif")




# Plot Y: Average yearly cost of use (all items, active and divested), X: Category use (to date)
p <- ggplot(
    usetodate, 
    aes(x = category_use, y = yearly_cost, colour = category)) +
    geom_point(show.legend = TRUE, aes(size = category_value)) +
    scale_x_continuous(limits=c(NA,NA)) +
    scale_y_continuous(limits=c(NA,1000)) +
    scale_color_manual(name = "Category", values = category_colors) +
    scale_size(range = c(1, 10)) +
    guides(size = FALSE) +
    labs(x = "Category daily use", y = "Average yearly cost of use (all items)") +
    ease_aes('linear')

# Animate and save plot
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 1000, width = 1200, nframes = 500, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Yearly_cost_and_Category_use-point.gif")







################################################################################################
######################################## CATEGORY PLOTS ########################################
################################################################################################

### The following functions are used to set up standard category plots
### Image plot: setup_category_plot_image(plot_data, cat, xmax, ymax, log_trans=TRUE, animated=FALSE)
### Point plot: setup_category_plot_point(plot_data, cat, xmax, ymax, log_trans=TRUE, avg_lines=TRUE)


## SET UP CATEGORY PLOTS MASTER DATA

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


### STANDARD CATEGORY IMAGE PLOTS ###

# Jackets and hoodies
p <- plot_data %>% setup_category_plot_image("Jackets and hoodies", xmax = 14, ymax = 32, log_trans=TRUE)
ggsave(filename = "Plots/Category-Jackets-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Blazers and vests
p <- plot_data %>% setup_category_plot_image("Blazers and vests", xmax = 1.5, ymax = 64, log_trans=TRUE)
ggsave(filename = "Plots/Category-Blazers-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Knits
p <- plot_data %>% setup_category_plot_image(cat = "Knits", xmax = 0.35, ymax = 20, log_trans=TRUE)
ggsave(filename = "Plots/Category-Knits-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Shirts
p <- plot_data %>% setup_category_plot_image(cat = "Shirts", xmax = 3.0, ymax = 20, log_trans=TRUE)
ggsave(filename = "Plots/Category-Shirts-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# T-shirts and tanks
p <- plot_data %>% setup_category_plot_image(cat = "T-shirts and tanks", xmax = 3, ymax = 16, log_trans=TRUE)
ggsave(filename = "Plots/Category-T-shirts-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Pants
p <- plot_data %>% setup_category_plot_image(cat = "Pants", xmax = 8, ymax = 12, log_trans=TRUE)
ggsave(filename = "Plots/Category-Pants-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Shorts
p <- plot_data %>% setup_category_plot_image(cat = "Shorts", xmax = 5, ymax = 16, log_trans=TRUE)
ggsave(filename = "Plots/Category-Shorts-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Belts
p <- plot_data %>% setup_category_plot_image(cat = "Belts", xmax = 9, ymax = 64, log_trans=TRUE)
ggsave(filename = "Plots/Category-Belts-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Socks
p <- plot_data %>% setup_category_plot_image(cat = "Socks", xmax = 2.5, ymax = 16, log_trans=TRUE)
ggsave(filename = "Plots/Category-Socks-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Shoes
p <- plot_data %>% setup_category_plot_image("Shoes", xmax = 8, ymax = 16, log_trans=TRUE)
ggsave(filename = "Plots/Category-Shoes-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Underwear shirts
p <- plot_data %>% setup_category_plot_image("Underwear shirts", xmax = 3, ymax = 8, log_trans=TRUE)
ggsave(filename = "Plots/Category-Underwear_shirts-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Underwear boxers
p <- plot_data %>% setup_category_plot_image("Underwear boxers", xmax = 3.5, ymax = 10, log_trans=TRUE)
ggsave(filename = "Plots/Category-Underwear_boxers-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Sportswear
p <- plot_data %>% setup_category_plot_image("Sportswear", xmax = 3.5, ymax = 64, log_trans=TRUE)
ggsave(filename = "Plots/Category-Sportswear-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())


# Multi category
p <- plot_data %>% setup_category_plot_image(c("Underwear shirts", "Underwear boxers"), xmax = 3.5, ymax = 10, log_trans=TRUE)
ggsave(filename = "Plots/Category-Multiple-image.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

dev.off()


## ANIMATED IMAGE PLOTS ##

animation <- plot_data %>% setup_category_plot_image("Shoes", xmax = 10, ymax = 16, log_trans=TRUE, animate=TRUE) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')
animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Shoes-image-animation.gif")

animation <- plot_data %>% setup_category_plot_image("Pants", xmax = 8, ymax = 12, log_trans=TRUE, animate=TRUE) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')
animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Pants-image-animation.gif")

animation <- plot_data %>% setup_category_plot_image("Underwear boxers", xmax = 3.5, ymax = 10, log_trans=TRUE, animate=TRUE) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')
animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Underwear_boxers-image-animation.gif")




### STANDARD CATEGORY POINT PLOTS ###

# Shirts, Pants, Shoes
p <- plot_data %>% setup_category_plot_point(categories = c("Shirts", "Pants", "Shoes"), xmax = 4, ymax = 5, log_trans=FALSE)
ggsave(filename = "Plots/Category-Shirts_Pants_Shoes-point.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# All items
p <- plot_data %>% setup_category_plot_point(categories = names(category_colors), xmax = 8, ymax = 64, log_trans=TRUE, avg_lines=FALSE)
ggsave(filename = "Plots/Category-All-point.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Experimentation
p <- plot_data %>% setup_category_plot_point(categories = c("Underwear shirts", "Underwear boxers"), xmax = 3.5, ymax = 2, log_trans=FALSE)
p <- plot_data %>% setup_category_plot_point(categories = c("Shoes"), xmax = 10, ymax = 16, log_trans=TRUE)

dev.off()


## ANIMATED CATEGORY POINT PLOTS ##

# Shirts, Pants, Shoes
animation <- plot_data %>% setup_category_plot_point(c("Shirts", "Pants", "Shoes"), xmax = 4, ymax = 5, log_trans=TRUE, animate=TRUE) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')
animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Shirts_Pants_Shoes-point-animation.gif")

# Underwear shirts, Underwear boxers, Socks
animation <- plot_data %>% setup_category_plot_point(c("Underwear shirts", "Underwear boxers", "Socks"), xmax = 4, ymax = 3, log_trans=TRUE, animate=TRUE) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')
animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Shirts_Underwear_and_Socks-point-animation.gif")






################################################################################################
######################################### DEVELOPMENT ##########################################
################################################################################################

## Plot Cost per use vs Cumulative use - Last date

# Shoes
p <- plot_data %>% setup_category_cumulative_plot_image("Shoes", xmax = 320, ymax = 16, log_trans=TRUE, trails=TRUE)
ggsave(filename = "Plots/Category-Cost_and_Cumulative_use-Shoes.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Belts
p <- plot_data %>% setup_category_cumulative_plot_image("Belts", xmax = 350, ymax = 100, log_trans=TRUE, trails=TRUE)
ggsave(filename = "Plots/Category-Cost_and_Cumulative_use-Belts.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

# Belts and Shoes
p <- plot_data %>% setup_category_cumulative_plot_image(c("Belts", "Shoes"), xmax = 350, ymax = 100, log_trans=TRUE, trails=TRUE)
ggsave(filename = "Plots/Category-Cost_and_Cumulative_use-Belts_and_Shoes.png", p, width = 10, height = 10, dpi = 300, units = "in", device=png())

dev.off()



# Cost per use x Cumulative use - Animated (DEVELOP trails support in animation)
animation <- plot_data %>% filter(category == 'Shoes') %>%
  ggplot(aes(x = cumuse, y = cost_per_use)) +
  geom_image(aes(image = photo), size = 0.08) +
  scale_x_continuous(limits=c(0,320)) +
  labs(x = "Cumulative times used", y = "Cost per use (€)") +
  scale_y_continuous(trans="log10", limits=c(NA,16)) +
  transition_time(date) + labs(title = "Date: {frame_time}") + ease_aes('linear')

animate(animation, height = 1000, width = 1000, nframes = 100, fps = 24, end_pause = 72)
#animate(animation, height = 1000, width = 1000, nframes = length(daterange), fps = 24, end_pause = 72)
anim_save("Plots/Category-Shoes-Cumulative_use-image-animation.gif")










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



