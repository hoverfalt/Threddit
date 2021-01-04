### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to set computing and plotting up environment
### Input: None


# Set up computing and plotting environment in global variables
set_up_environment <- function(){
  
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
  source("05-Build_plots.R")
  
  
  ## Set up static variables
  
  # Set category order (DEPENDENCY)
  category_order <<- c("Jackets and hoodies", "Blazers and vests", "Knits", "Shirts", "T-shirts and tanks", "Pants",
                     "Shorts", "Belts", "Socks", "Shoes", "Underwear shirts", "Underwear boxers", "Sportswear")
  
  # Set gray and white theme
  theme_set(theme_gray())
  
  # Set plot palette for 13 categories
  category_colors <<- c('#960001', '#FC3334', '#FF9A02', '#FFDB05', '#4CDA00', '#00B0F0',
                       '#0070C0', '#002060', '#A860E9', '#7030A0', '#A5A5A5', '#7B7B7B', '#444444')
  
  # Set color names by category name for consistent category colors in plots
  names(category_colors) <- category_order
  
  # Set category photos
  category_photos <<- data.frame(
    "category" = levels(plotuse$category),
    "photo" = c('Photos/Category_image-Jackets_and_hoodies.png',
                'Photos/Category_image-Blazers_and_vests.png',
                'Photos/Category_image-Knits.png',
                'Photos/Category_image-Shirts.png',
                'Photos/Category_image-T-shirts_and_tanks.png',
                'Photos/Category_image-Pants.png',
                'Photos/Category_image-Shorts.png',
                'Photos/Category_image-Belts.png',
                'Photos/Category_image-Socks.png',
                'Photos/Category_image-Shoes.png',
                'Photos/Category_image-Underwear_shirts.png',
                'Photos/Category_image-Underwear_boxers.png',
                'Photos/Category_image-Sportswear.png'))
  
  # Set rolling average window size to 30 days
  rolling_average_window <<- 30
  
  # Set guides for daily cost vs cumulative use plots
  guides_prices <<- c(5, 10, 20, 50, 100, 200, 400, 800)

  # Set logarithmic y scale breaks
  log_breaks <<- c(0.05, 0.1, 0.25, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 400, 800)
    
  # Set author label to add in the upper right corner of plots
  author_label <<- "hoverfalt.github.io"

  # Set Dropbox Threddit photo links path identifier used to filter our non-Threddit links 
  Threddit_Dropbox_path_identifier<- "/thredditr/"
  
  
  # Set Google API Key to use Google Sheets API 
  set_Google_API_key()
  
  # Load item photo URL data frame
  load("Data/item_photo_URLs.Rda")
  item_photo_URLs <<- item_photo_URLs
}




