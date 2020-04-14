### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to process and plot Threddit data
### Input: Excel file with specific structure and formatting


####################################################
################ SET UP ENVIRONMENT ################
####################################################

# Remove all objects from workspace
rm(list = ls())

# Load required packages
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(htmlwidgets)
library(plotly)
library(gganimate)
library(gifski)
library(ggimage)
library(roll)

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
                   "Socks", "Shoes", "Underwear shirts", "Underwear boxers")


################################################################
################ READ AND PROCESS STANDARD DATA ################
################################################################

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


### Save master plotting data to file to avoid 5 min reprocessing

# Save tidy data data.frame to file for easier retrieval
save(plotuse,file="Data/Threddit-plotuse-2020-04-14.Rda")

# Load data from file
load("Data/Threddit-plotuse-2020-04-14.Rda")


###########################################################
############# SET UP PLOTTING ENVIRONMENT #################
###########################################################

### This section sets up the plotting environment and functions 

# Set gray and white theme
theme_set(theme_gray())

# Set plot palette 12 categories
category_colors <- c('#960001', '#FC3334', '#FF9A02', '#FFDB05', '#4CDA00', '#00B0F0',
                      '#0070C0', '#002060', '#A860E9', '#7030A0', '#A5A5A5', '#7B7B7B')

# Set color names by category name for consistend category colors in plots
names(category_colors) <- levels(plotuse$category)


#########################################################
#################### PORTFOLIO PLOTS ####################
#########################################################

### Calculate active inventory item count and value by category
inventory <- calculate_portfolio_plot_data(plotuse)


################################
### Standard portfolio plots ###
################################


# Plot active inventory item count by category
p <- inventory %>% 
    ggplot( aes(x = date, y = itemcount, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory (number of items)")
p

# Save plot to file (300x250mm at 300dpi)
ggsave(filename = "Plots/Threddit-line_plot-Active_inventory-300x250mm-300dpi.png", p,
     width = 300, height = 250, dpi = 300, units = "mm", device='png')


# Plot active inventory value by category (line plot)
p <- inventory %>% 
    ggplot( aes(x = date, y = categoryvalue, colour = category)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
p

# Plot active inventory value by category (stacked area plot)
p <- inventory %>% 
    ggplot( aes(x = date, y = categoryvalue, fill = category)) + geom_area() +
    scale_fill_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
p

# Save plot to file (300x250mm at 300dpi)
ggsave(filename = "Plots/Threddit-area_plot-Active_inventory-value-300x250mm-300dpi-x.png", p,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')


### Plot total active inventory value

# Calculate toal active inventory (this cannot be included in inventory as it is portfolio-level data)
inventory_value_total <- plotuse %>%
    filter(active == TRUE) %>%
    merge(itemvalues, all = TRUE) %>%
    group_by(date) %>%
    summarise(inventoryvalue = sum(price)) 

p <- inventory_value_total %>% 
    ggplot( aes(x = date, y = inventoryvalue)) + geom_line() +
    scale_color_manual(name = "Category", values = category_colors) +
    labs(x = "Date", y = "Active inventory value at purchase price (€)")
p

# Save plot to file (300x250mm at 300dpi)
ggsave(filename = "Plots/Threddit-line_plot-Active_inventory-total-value-300x250mm-300dpi.png", p,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')


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

# Save plot to file (300x250mm at 300dpi)
ggsave(filename = "Plots/Threddit-line_plot-Monthly_inventory-turnaround-300x250mm-300dpi.png", p,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')


### Daily cost with rolling average ###

# Calculate daily cost and rolling daily cost in 90-day window 
daily_cost <- plotuse %>%
    filter(used == TRUE) %>%
    select(item, date, cost_per_use) %>%
    group_by(item) %>% mutate(daily_cost = min(cost_per_use)) %>%
    group_by(date) %>% summarise(daily_cost = sum(daily_cost)) %>%
    arrange(date) %>% mutate(average_daily_cost = roll_sum(daily_cost, 90)/90)

p <- ggplot(daily_cost, aes(x = date, y = average_daily_cost)) +
    geom_line() +
    scale_y_continuous(limits=c(0,NA)) +
    labs(x = "Date", y = "90-day rolling average of daily cost")
p

# Save plot to file (300x250mm at 300dpi)
ggsave(filename = "Threddit-line_plot-Daily_cost-300x250mm-300dpi.png", p,
       width = 300, height = 250, dpi = 300, units = "mm", device='png')


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

# Animate plot
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 600, width = 700, nframes = 404, fps = 24, end_pause = 72)
#animate(animation, height = 800, width = 900, nframes = 202, fps = 10, end_pause = 72)

# Save animation to file
anim_save("Threddit-animation-Category-Avgerage_daily_cost-vs-category_daily_use-point-600x700-24fps-404-frames-x.gif")


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

# Animate plot
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 600, width = 700, nframes = 404, fps = 24, end_pause = 72)
#animate(animation, height = 800, width = 900, nframes = 202, fps = 10, end_pause = 72)

# Save animation to file
anim_save("Threddit-animation-Category-Avgerage_yearly_cost-vs-category_daily_use-point-600x700-24fps-404-frames-x.gif")



########################################################
#################### CATEGORY PLOTS ####################
########################################################

### The following functions are used to set up standard category plots
### Point plot: setup_plot_point(plot_data, xmax, ymax, log_trans=TRUE)
### Image plot: setup_plot_image(plot_data, xmax, ymax, log_trans=TRUE)


### Plot point - All ###
plot_data <- plotuse %>% filter(days_active >= 30)
p <- setup_plot_point(plot_data, xmax = 10, ymax = 20, log_trans = TRUE)
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 600, width = 700, nframes = 120, fps = 10)


### Plot point - Single category ###
plot_data <- plotuse %>% filter(category == 'Shirts' & days_active >= 30)
p <- setup_plot_point(plot_data, xmax = 3, ymax = 15, log_trans = FALSE)

animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 600, width = 700, nframes = 120, fps = 10)
animate(animation, height = 600, width = 700, nframes = 60, fps = 10)


### Plot image - Single category ###
plot_data <- plotuse %>% filter(category == 'Knits' & days_active >= 30)
p <- setup_plot_image(plot_data, xmax = 5, ymax = 100, log_trans = TRUE)
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 1000, width = 1100, nframes = 120, fps = 24)


### Plot mutiple categories
unique(plotuse$category)
categories_to_plot <- c('Underwear shirts', 'Underwear boxers', 'Socks')
plot_data <- plotuse %>% filter(category %in% categories_to_plot & days_active >= 30)
p <- setup_plot_point(plot_data, xmax = 5, ymax = 15, log_trans = TRUE)


### Plot last date only
plot_data <- plotuse %>% filter(category == 'Shorts' & days_active >= 30 & date == max(plotuse$date))
plot_data <- plotuse %>% filter(days_active >= 30 & date == max(plotuse$date))
p <- setup_plot_image(plot_data, xmax = 4, ymax = 20, log_trans = TRUE)
p

#######################################################################
### Plot single category with vertical and horizontal average lines ###
#######################################################################

unique(itemuse$category)

### Set data and line plot color by category

# Full time range
plot_data <- plotuse %>% filter(category == 'Jackets and hoodies' & days_active >= 30)
category_color <- category_colors["Jackets and hoodies"]

# Last date only
plot_data <- plotuse %>% filter(category == 'Jackets and hoodies' & days_active >= 30 & date == max(plotuse$date))
category_color <- category_colors["Jackets and hoodies"]


# Group plot data by animation frame (date)
plot_data <- plot_data %>% group_by(date)

# Calculate variable averages by group i.e. frame (day). Include only one item per frame, rest become NA
avg_merge_divested <- plot_data %>% filter(active == FALSE) %>%
    mutate(avg_use_per_month_divested = mean(use_per_month), avg_cost_per_use_divested = mean(cost_per_use)) %>% slice(1)
avg_merge_active <- plot_data %>% filter(active == TRUE) %>%
    mutate(avg_use_per_month_active = mean(use_per_month), avg_cost_per_use_active = mean(cost_per_use)) %>% slice(1)
avg_merge <- rbind(avg_merge_divested, avg_merge_active)
plot_data <- merge(plot_data, avg_merge, all = TRUE)

# Clear temp variables from memory
avg_merge_active <- NULL
avg_merge_divested <- NULL
avg_merge <- NULL

# Set up single-category plot with x and y lines showing average of active and divested items
p <- setup_plot_point(plot_data, xmax = 5, ymax = 100, log_trans = TRUE) +
    geom_vline(aes(xintercept = avg_use_per_month_divested), colour = category_color, linetype = "dotted") +
    geom_hline(aes(yintercept = avg_cost_per_use_divested), colour = category_color, linetype = "dotted") +
    geom_vline(aes(xintercept = avg_use_per_month_active), colour = category_color, linetype = "dotted", alpha = 0.4) +
    geom_hline(aes(yintercept = avg_cost_per_use_active), colour = category_color, linetype = "dotted", alpha = 0.4)
p

# Animate with frame = date
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")

# 600 resolution, 202 frames, 10 fps
animate(animation, height = 600, width = 700, nframes = 202, fps = 10)
# 600 resolution, 404 frames, 24 fps
animate(animation, height = 600, width = 700, nframes = 404, fps = 24, end_pause = 72)
# 1000 resolution, 404 frames, 24 fps
animate(animation, height = 1000, width = 1100, nframes = 404, fps = 24, end_pause = 72)


###################################################################
#################### Animating and saving plots ####################
###################################################################

# Without trails
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}")
animate(animation, height = 600, width = 600, nframes = 120, fps = 10)

# With trails
animation <- p + transition_time(date) + labs(title = "Date: {frame_time}") +
    shadow_mark(alpha = 0.1, size = 0.5)
animate(animation, height = 800, width = 1000, nframes = 120, fps = 10)

# Calculate total number of frames with current data date range
as.integer((max(plotuse$date)-min(plotuse$date))) 

# Save animation to file
anim_save("Threddit-animation-Cost_per_use-vs-Days_active-log-lin-point-Socks-600x700-24fps-404-frames-x.gif")

# View follow
view_follow(fixed_y = TRUE)


###################################################
#################### Resources ####################
###################################################


# dplyr cheat sheet
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

# GGANIMATE: HOW TO CREATE PLOTS WITH BEAUTIFUL ANIMATION IN R
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/


#start_time <- Sys.time()
#end_time <- Sys.time()
#print(end_time - start_time)
#rm(start_time)
#rm(end_time)

