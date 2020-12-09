### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to build plots


### PREPARE PLOT DATA #################################################################################################

calculate_image_plot_master_data <- function(){

  ## Set up portfolio plots master data ##
  
  # Calculate active inventory item count and value by category (global variable)
  inventory <<- calculate_portfolio_plot_data(plotuse)
  
  # Calculate daily cost and rolling average (Sportswear excluded by default, can be overridden)
  daily_cost <<- calculate_daily_cost(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")
  
  # Calculate complete portfolio data for plottin Daily/Yearly cost vs Category use
  usetodate <<- calculate_complete_portfolio_plot_data(plotuse)
  # Add category photos to data frame
  usetodate_anim <<- merge(usetodate, category_photos, by.x = "category")
  
  
  
  ## Set up category plots master data ##
  
  # All following plotting sections rely on this data to be prepared first.
  
  # Create category plots master data and group by date to calculate averages
  plot_data <<- plotuse %>% group_by(category, date)
  
  # Calculate use weighted average use per month and average cost per use for divested items
  avg_merge_divested <- plot_data %>% filter(active == FALSE &  days_active >= 30) %>%
    select(category, item, date, use_per_month, cost_per_use, days_active, cumuse) %>%
    mutate(avg_use_per_month_divested = sum(cumuse) / sum(days_active) * 30.5, avg_cost_per_use_divested = sum(cost_per_use * cumuse) / sum(cumuse)) %>%
    select(-use_per_month, -cost_per_use, -days_active, -cumuse)
  plot_data <<- merge(plot_data, avg_merge_divested, all = TRUE)
  rm(avg_merge_divested)
  plot_data <<- plot_data %>% ungroup()
  
  # Create data set (plot_data_reduced) suitable for animation
  # Reduce dates (frames) to half for animation: remove every second date (c(T,F) replicates automatically)
  # Animation used transition_state(), which avoids rendering multiple dates in one frame, but also
  # causes the number of frames to determine the dates to be included. Thus the number of frames needs to
  # match the amount of dates to be animated. Reducing the amount of dates (frames) is the best way.
  plot_data_reduced <<- plot_data[plot_data$date %in% unique(plot_data$date)[c(TRUE, FALSE)],]
  
}






### STANDARD PLOTS #################################################################################################

# The file copying and removing is a workaround to keep some Dropbox links from breaking when the file is rewritten by png()
# This is rather clumsy, but solves the problem until a better solution is found

build_standard_plots <- function(){

  ## PORTFOLIO PLOTS 
    
  # Portfolio plot: Active inventory item count by category
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_item_count_plot()
  ggsave(filename = "Plots/Portfolio-Inventory-Item_count.png", p, width = 12, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Inventory-Item_count.png", "Website/Plots/Portfolio-Inventory-Item_count.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Inventory-Item_count.png")

  # Portfolio plot: Active inventory value by category (line plot)
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_by_category_plot()
  ggsave(filename = "Plots/Portfolio-Inventory-Value_by_category.png", p, width = 12, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Inventory-Value_by_category.png", "Website/Plots/Portfolio-Inventory-Value_by_category.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Inventory-Value_by_category.png")

  # Portfolio plot: Active inventory value by category (stacked area plot)
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_stacked_plot()
  ggsave(filename = "Plots/Portfolio-Inventory-Value_stacked.png", p, width = 12, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Inventory-Value_stacked.png", "Website/Plots/Portfolio-Inventory-Value_stacked.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Inventory-Value_stacked.png")

  # Portfolio plot: Average DAILY cost vs category use
  p <- usetodate_anim %>% setup_daily_cost_and_category_use_plot(ybreaks = 2, animate = FALSE)
  ggsave(filename = "Plots/Portfolio-Daily_cost_and_Category_use.png", p, width = 12, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Daily_cost_and_Category_use.png", "Website/Plots/Portfolio-Daily_cost_and_Category_use.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Daily_cost_and_Category_use.png")

  # Portfolio plot: Average YEARLY cost vs category use
  p <- usetodate_anim %>% setup_yearly_cost_and_category_use_plot(ybreaks = 100, animate = FALSE)
  ggsave(filename = "Plots/Portfolio-Yearly_cost_and_Category_use.png", p, width = 12, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Yearly_cost_and_Category_use.png", "Website/Plots/Portfolio-Yearly_cost_and_Category_use.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Yearly_cost_and_Category_use.png")

  # Portfolio plot: daily cost and rolling average
  p <- setup_daily_cost_plot(daily_cost, ymax = 40, ybreaks = 5, seasons = TRUE)
  ggsave(filename = "Plots/Portfolio-Daily_cost-plot.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Portfolio-Daily_cost-plot.png", "Website/Plots/Portfolio-Daily_cost-plot.png", overwrite = TRUE)
  file.remove("Plots/Portfolio-Daily_cost-plot.png")

  

  ## CATEGORY PLOTS

  # Standard category plots
  for (i in category_order){
    p <- plot_data %>% setup_category_plot_image(categories = c(i), xmax = NA, ymax = NA, ybreaks = log_breaks, log_trans=TRUE)
    ggsave(filename = paste("Plots/Category-", gsub(" ", "_", i), ".png", sep=""),
           p, width = 10, height = 10, dpi = 300, units = "in")
    file.copy(paste("Plots/Category-", gsub(" ", "_", i), ".png", sep=""),
              paste("Website/", paste("Plots/Category-", gsub(" ", "_", i), ".png", sep=""), sep=""),
              overwrite = TRUE)
    file.remove(paste("Plots/Category-", gsub(" ", "_", i), ".png", sep=""))
  }

  
  
  ## Category plot - Cost per use vs Cumulative use
  
  # Jackets and hoodies
  p <- plot_data %>% setup_category_cumulative_plot_image("Jackets and hoodies", xmax = 400, ymax = 10, ybreaks = log_breaks, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Jackets_and_hoodies-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Jackets_and_hoodies-Cost_and_Cumulative_use.png", "Website/Plots/Category-Jackets_and_hoodies-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Jackets_and_hoodies-Cost_and_Cumulative_use.png")

  # Blazers and vests
  p <- plot_data %>% setup_category_cumulative_plot_image("Blazers and vests", xmax = 60, ymax = 50, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Blazers_and_vests-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Blazers_and_vests-Cost_and_Cumulative_use.png", "Website/Plots/Category-Blazers_and_vests-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Blazers_and_vests-Cost_and_Cumulative_use.png")

  # Knits
  p <- plot_data %>% setup_category_cumulative_plot_image("Knits", xmax = 30, ymax = 50, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Knits-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Knits-Cost_and_Cumulative_use.png", "Website/Plots/Category-Knits-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Knits-Cost_and_Cumulative_use.png")

  # Shirts
  p <- plot_data %>% setup_category_cumulative_plot_image("Shirts", xmax = 60, ymax = 45, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Shirts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shirts-Cost_and_Cumulative_use.png", "Website/Plots/Category-Shirts-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Shirts-Cost_and_Cumulative_use.png")

  # T-shirts and tanks
  p <- plot_data %>% setup_category_cumulative_plot_image("T-shirts and tanks", xmax = 110, ymax = 20, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-T-shirts_and_tanks-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-T-shirts_and_tanks-Cost_and_Cumulative_use.png", "Website/Plots/Category-T-shirts_and_tanks-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-T-shirts_and_tanks-Cost_and_Cumulative_use.png")

  # Pants
  p <- plot_data %>% setup_category_cumulative_plot_image("Pants", xmax = 120, ymax = 35, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Pants-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Pants-Cost_and_Cumulative_use.png", "Website/Plots/Category-Pants-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Pants-Cost_and_Cumulative_use.png")

  # Shorts
  p <- plot_data %>% setup_category_cumulative_plot_image("Shorts", xmax = 160, ymax = 100, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Shorts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shorts-Cost_and_Cumulative_use.png", "Website/Plots/Category-Shorts-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Shorts-Cost_and_Cumulative_use.png")

  # Belts
  p <- plot_data %>% setup_category_cumulative_plot_image("Belts", xmax = 220, ymax = 100, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Belts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Belts-Cost_and_Cumulative_use.png", "Website/Plots/Category-Belts-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Belts-Cost_and_Cumulative_use.png")

  # Socks
  p <- plot_data %>% setup_category_cumulative_plot_image("Socks", xmax = 50, ymax = 10, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Socks-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Socks-Cost_and_Cumulative_use.png", "Website/Plots/Category-Socks-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Socks-Cost_and_Cumulative_use.png")

  # Shoes
  p <- plot_data %>% setup_category_cumulative_plot_image("Shoes", xmax = 340, ymax = 30, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Shoes-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shoes-Cost_and_Cumulative_use.png", "Website/Plots/Category-Shoes-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Shoes-Cost_and_Cumulative_use.png")

  # Underwear shirts
  p <- plot_data %>% setup_category_cumulative_plot_image("Underwear shirts", xmax = 45, ymax = 42, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Underwear_shirts-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Underwear_shirts-Cost_and_Cumulative_use.png", "Website/Plots/Category-Underwear_shirts-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Underwear_shirts-Cost_and_Cumulative_use.png")

  # Underwear boxers
  p <- plot_data %>% setup_category_cumulative_plot_image("Underwear boxers", xmax = 35, ymax = 16, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Underwear_boxers-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Underwear_boxers-Cost_and_Cumulative_use.png", "Website/Plots/Category-Underwear_boxers-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Underwear_boxers-Cost_and_Cumulative_use.png")

  # Sportswear
  p <- plot_data %>% setup_category_cumulative_plot_image("Sportswear", xmax = 70, ymax = 35, log_trans=TRUE, trails=TRUE, guides=TRUE)
  ggsave(filename = "Plots/Category-Sportswear-Cost_and_Cumulative_use.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Sportswear-Cost_and_Cumulative_use.png", "Website/Plots/Category-Sportswear-Cost_and_Cumulative_use.png", overwrite = TRUE)
  file.remove("Plots/Category-Sportswear-Cost_and_Cumulative_use.png")


  
  ## Category plot - Times used

  for (i in category_order){
    p <- setup_category_times_used_plot(plot_data, categories = c(i), animate = FALSE)
    ggsave(filename = paste("Plots/Category-", gsub(" ", "_", i), "-Times_used.png", sep=""),
           p, width = 10, height = 10, dpi = 300, units = "in")
    file.copy(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used.png", sep=""),
              paste("Website/", paste("Plots/Category-", gsub(" ", "_", i), "-Times_used.png", sep=""), sep=""),
              overwrite = TRUE)
    file.remove(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used.png", sep=""))
  }

  
  
  ## Category plot: daily cost with rolling average
  
  # Plot: Jackets and hoodies
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Jackets and hoodies")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 8, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Jackets_and_hoodies-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Jackets_and_hoodies-Daily_cost.png", "Website/Plots/Category-Jackets_and_hoodies-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Jackets_and_hoodies-Daily_cost.png")

  # Blazers and vests
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Blazers and vests")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 40, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Blazers_and_vests-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Blazers_and_vests-Daily_cost.png", "Website/Plots/Category-Blazers_and_vests-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Blazers_and_vests-Daily_cost.png")

  # Knits
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Knits")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 20, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Knits-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Knits-Daily_cost.png", "Website/Plots/Category-Knits-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Knits-Daily_cost.png")

  # Shirts
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shirts")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 20, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Shirts-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shirts-Daily_cost.png", "Website/Plots/Category-Shirts-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Shirts-Daily_cost.png")

  # T-shirts and tanks
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "T-shirts and tanks")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 8, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-T-shirts_and_tanks-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-T-shirts_and_tanks-Daily_cost.png", "Website/Plots/Category-T-shirts_and_tanks-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-T-shirts_and_tanks-Daily_cost.png")

  # Pants
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Pants")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 8, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Pants-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Pants-Daily_cost.png", "Website/Plots/Category-Pants-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Pants-Daily_cost.png")

  # Shorts
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shorts")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 8, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Shorts-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shorts-Daily_cost.png", "Website/Plots/Category-Shorts-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Shorts-Daily_cost.png")

  # Belts
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Belts")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 4, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Belts-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Belts-Daily_cost.png", "Website/Plots/Category-Belts-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Belts-Daily_cost.png")

  # Socks
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Socks")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 2, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Socks-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Socks-Daily_cost.png", "Website/Plots/Category-Socks-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Socks-Daily_cost.png")

  # Shoes
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Shoes")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 16, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Shoes-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Shoes-Daily_cost.png", "Website/Plots/Category-Shoes-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Shoes-Daily_cost.png")

  # Underwear shirts
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Underwear shirts")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 8, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Underwear_shirts-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Underwear_shirts-Daily_cost.png", "Website/Plots/Category-Underwear_shirts-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Underwear_shirts-Daily_cost.png")

  # Underwear boxers
  daily_cost_category <- calculate_daily_cost(plotuse, rolling_average_window, categories_include = "Underwear boxers")
  p <- setup_daily_cost_plot(daily_cost_category, ymax = 4, seasons = TRUE, legend = FALSE)
  ggsave(filename = "Plots/Category-Underwear_boxers-Daily_cost.png", p, width = 10, height = 10, dpi = 300, units = "in")
  file.copy("Plots/Category-Underwear_boxers-Daily_cost.png", "Website/Plots/Category-Underwear_boxers-Daily_cost.png", overwrite = TRUE)
  file.remove("Plots/Category-Underwear_boxers-Daily_cost.png")

  # Sportswear
  # Doesn't make sense
}





