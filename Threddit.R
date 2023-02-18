### Threddit.R - Olof Hoverfält - 2018-2023 - hoverfalt.github.io

# For data and insights: https://hoverfalt.github.io/
# For context and background: https://www.reaktor.com/blog/why-ive-tracked-every-single-piece-of-clothing-ive-worn-for-three-years/


# Functions to process and plot wardrobe performance data
# Input: Google Sheets with specific structure and formatting
# Output: Portfolio and category plots (png), animations (gif), and data tables (Rda) for publishing


### SET UP ENVIRONMENT ##########################################################################################

# Clear workspace
rm(list = ls())

# Set up computing and plotting environment
source("00-Set_up_environment.R")
set_up_environment()



### READ AND PROCESS DATA ##########################################################################################

## Read and clean master raw data
masterdata <- read_data_GD(get_Google_sheet_ID()) # Read master data from Google Drive
temp <- masterdata

## Transform data
plotuse <- transform_data(masterdata) %>% # 1) Transform raw data into tidy data
  calculate_active_use_data() %>% # 2) Calculate cumulative use data
  calculate_total_use_data() %>% # 3) Calculate total use data, including divested items 
  calculate_plot_data() # 4) Calculate plot data for the standard plots

# Save tidy data data.frame to file for easier retrieval (2023-02-16)
save(masterdata,file="Data/Threddit-masterdata.Rda")
save(plotuse,file="Data/Threddit-plotuse.Rda")
save(daterange,file="Data/Threddit-daterange.Rda")
# Load data from file
load("Data/Threddit-masterdata.Rda")
load("Data/Threddit-plotuse.Rda")
load("Data/Threddit-daterange.Rda")

# Calculate image plot master data (this is also required for animations)
calculate_image_plot_master_data()

#write.csv(plotuse,"Data/hoverfalt.github.io-use_data_long.csv", row.names = FALSE)
#write.csv(masterdata,"Data/hoverfalt.github.io-use_data_wide.csv", row.names = TRUE)


### REFRESH AND PUBLISH DATA AND PLOTS ##########################################################################

### Calculate item data tables
# Sets global variables item_listings, and category_listing
calculate_category_data_tables(plotuse, masterdata)

### Refresh Dropbox share links (this need to be done only when a new item has been added, or a link has been broken)
#refresh_share_links() 

### Build image plots 
build_standard_plots()





### ANIMATIONS ##################################################################################################

# Calculate and publich anumations
# Animations are excluded from the standard refresh-and-publish cycle as they take some 60+ minutes to compute


### DAILY COST WITH ROLLING AVERAGE ##########################################################################################

# Prepare data for animated daily cost plotting (HEAVY COMPUTING)
daily_cost_anim <- calculate_daily_cost_anim(plotuse, rolling_average_window, categories_include = category_order, categories_exclude = "Sportswear")

# Subset data to exclude NAs in 30-day rolling average (this is to avoid transition_time failure in animation)
daily_cost_anim_plot <- daily_cost_anim %>% filter(day >= daterange[rolling_average_window] & day <= daterange[length(daterange)-rolling_average_window])

# Reduce frames by removing every second day (note: not date!)
daily_cost_anim_plot_reduced <- daily_cost_anim_plot[daily_cost_anim_plot$day %in% unique(daily_cost_anim_plot$day)[c(TRUE, FALSE)],]

# Set up animation, animate, and save (HEAVY COMPUTING) - Latest: 2023-01-08
setup_daily_cost_animation(daily_cost_anim_plot_reduced, ymax = 40) %>%
animate(height = 1000, width = 1000, nframes = length(unique(daily_cost_anim_plot_reduced$day)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Daily_cost-animation.gif")
gcs_upload("Plots/Portfolio-Daily_cost-animation.gif", name="Portfolio-Daily_cost-animation.gif")

# Avoid GCS timeout
gcs_list_buckets(Firebase_project_id)



### AVERAGE DAILY/YEARLY COST vs CATEGORY USE ################################################################################

# Reduce frames by removing every second date
usetodate_anim_reduced <- usetodate_anim[usetodate_anim$date %in% unique(usetodate_anim$date)[c(TRUE, FALSE)],] # Reduce frames

# Animation: Average DAILY cost vs category use (HEAVY COMPUTING) - Latest: 2022-03-21
animation <- usetodate_anim_reduced %>% setup_daily_cost_and_category_use_plot(ymax = 15, ybreaks = 2, animate = TRUE)
animate(animation, height = 1000, width = 1150, nframes = length(unique(usetodate_anim_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Daily_cost_and_Category_use-animation.gif")
gcs_upload("Plots/Portfolio-Daily_cost_and_Category_use-animation.gif", name="Portfolio-Daily_cost_and_Category_use-animation.gif")

# Animation: Average YEARLY cost vs category use (HEAVY COMPUTING) - Latest: 2021-04-10
animation <- usetodate_anim_reduced %>% setup_yearly_cost_and_category_use_plot(ymax = 1000, ybreaks = 200, animate = TRUE)
animate(animation, height = 1000, width = 1150, nframes = length(unique(usetodate_anim_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Portfolio-Yearly_cost_and_Category_use-animation.gif")
gcs_upload("Plots/Portfolio-Yearly_cost_and_Category_use-animation.gif", name="Portfolio-Yearly_cost_and_Category_use-animation.gif")





## STANDARD CATEGORY IMAGE PLOTS ANIMATED ####################################################################################

# Jackets and hoodies
plot_data_reduced %>% setup_category_plot_image("Jackets and hoodies", xmax = 20, ymax = 16, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Jackets_and_hoodies-animation.gif")

# Blazers and vests
plot_data_reduced %>% setup_category_plot_image("Blazers and vests", xmax = 2, ymax = NA, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Blazers_and_vests-animation.gif")

# Knits
plot_data_reduced %>% setup_category_plot_image("Knits", xmax = 1.0, ymax = 32, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Knits-animation.gif")

# Shirts
plot_data_reduced %>% setup_category_plot_image("Shirts", xmax = 3.5, ymax = 70, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Shirts-animation.gif")

# T-shirts and tanks
plot_data_reduced %>% setup_category_plot_image("T-shirts and tanks", xmax = 3, ymax = 16, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-T-shirts_and_tanks-animation.gif")

# Pants
plot_data_reduced %>% setup_category_plot_image("Pants", xmax = 20, ymax = 32, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Pants-animation.gif")

# Shorts
plot_data_reduced %>% setup_category_plot_image("Shorts", xmax = 8, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Shorts-animation.gif")


# Belts
plot_data_reduced %>% setup_category_plot_image("Belts", xmax = 9, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Belts-animation.gif")

# Socks
plot_data_reduced %>% setup_category_plot_image("Socks", xmax = 3, ymax = 10, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Socks-animation.gif")

# Shoes
plot_data_reduced %>% setup_category_plot_image("Shoes", xmax = 11, ymax = 100, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Shoes-animation.gif")

# Underwear shirts
plot_data_reduced %>% setup_category_plot_image("Underwear shirts", xmax = 3, ymax = 20, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Underwear_shirts-animation.gif")

# Underwear boxers
plot_data_reduced %>% setup_category_plot_image("Underwear boxers", xmax = 3.5, ymax = 20, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Underwear_boxers-animation.gif")

# Sportswear
plot_data_reduced %>% setup_category_plot_image("Sportswear", xmax = 6, ymax = 50, log_trans=TRUE, animate=TRUE) %>% 
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72)
anim_save("Plots/Category-Sportswear-animation.gif")


# Quick fix for resetting failing protocol state
gcs_list_buckets(Firebase_project_id)

# Upload animations to Google Cloud (THIS MIGHT BE EASIER BY HAND)
gcs_upload("Plots/Category-Jackets_and_hoodies-animation.gif", name="Category-Jackets_and_hoodies-animation.gif")
gcs_upload("Plots/Category-Blazers_and_vests-animation.gif", name="Category-Blazers_and_vests-animation.gif")
gcs_upload("Plots/Category-Knits-animation.gif", name="Category-Knits-animation.gif")
gcs_upload("Plots/Category-Shirts-animation.gif", name="Category-Shirts-animation.gif")
gcs_upload("Plots/Category-T-shirts_and_tanks-animation.gif", name="Category-T-shirts_and_tanks-animation.gif")
gcs_upload("Plots/Category-Pants-animation.gif", name="Category-Pants-animation.gif")
gcs_upload("Plots/Category-Shorts-animation.gif", name="Category-Shorts-animation.gif")
gcs_upload("Plots/Category-Belts-animation.gif", name="Category-Belts-animation.gif")
gcs_upload("Plots/Category-Socks-animation.gif", name="Category-Socks-animation.gif")
gcs_upload("Plots/Category-Shoes-animation.gif", name="Category-Shoes-animation.gif")
gcs_upload("Plots/Category-Underwear_shirts-animation.gif", name="Category-Underwear_shirts-animation.gif")
gcs_upload("Plots/Category-Underwear_boxers-animation.gif", name="Category-Underwear_boxers-animation.gif")
gcs_upload("Plots/Category-Sportswear-animation.gif", name="Category-Sportswear-animation.gif")




### CATEGORY PLOT - TIMES USED ###

# ANIMATED PLOTS #

# Reduce dates (frames) to half for animation: remove every second date (c(T,F) replicates automatically)
# Animation used transition_state(), which avoids rendering multiple dates in one frame, but also
# causes the number of frames to determine the date to be included. Thus the number of frames needs to
# match the amount of dates to be animated. Reducing the amount of dates (frames) is the best way.

# Reduce frames
plot_data_reduced <- plot_data[plot_data$date %in% unique(plot_data$date)[c(TRUE, FALSE)],]

# Create and save animations for all categories (HEAVY COMPUTING ~ 12 x 4-8 min = 1h)
#for (i in category_order){
for (i in category_order[4]){
  setup_category_times_used_plot(plot_data_reduced, categories = c(i), animate = TRUE) %>%
    animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72) # Frames = states + end pause
  anim_save(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""))
  gcs_upload(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""),
             name=paste("Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""))
}


# Animating only selected categories
i <- category_order[13]
i <- "Blazers and vests"
setup_category_times_used_plot(plot_data_reduced, categories = c(i), animate = TRUE) %>%
  animate(height = 1000, width = 1000, nframes = length(unique(plot_data_reduced$date)) + 72, fps = 24, end_pause = 72) # Frames = states + end pause
anim_save(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""))
file.copy(paste("Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""),
          paste("Website/", paste("Plots/Category-", gsub(" ", "_", i), "-Times_used-animation.gif", sep=""), sep=""),
          overwrite = TRUE)






















##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### DEVELOPMENT ##### ##### ##### ##### ##### ##### #####
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

# Circular combinatorics plotting sandbox
## https://github.com/jokergoo/circlize

library(circlize)

mat = matrix(sample(100, 25), 5)
rownames(mat) = letters[1:5]
colnames(mat) = letters[1:5]
chordDiagram(mat, directional = FALSE, transparency = 0.5)
circos.clear()




df = read.table(textConnection("
 brand_from model_from brand_to model_to
      VOLVO        s80      BMW  5series
        BMW    3series      BMW  3series
      VOLVO        s60    VOLVO      s60
      VOLVO        s60    VOLVO      s80
        BMW    3series     AUDI       s4
       AUDI         a4      BMW  3series
       AUDI         a5     AUDI       a5
"), header = TRUE, stringsAsFactors = FALSE)


brand = c(structure(df$brand_from, names=df$model_from), structure(df$brand_to,names= df$model_to))
brand = brand[!duplicated(names(brand))]
brand = brand[order(brand, names(brand))]
brand_color = structure(2:4, names = unique(brand))
model_color = structure(2:8, names = names(brand))

gap.after = do.call("c", lapply(table(brand), function(i) c(rep(2, i-1), 8)))
circos.par(gap.after = gap.after, cell.padding = c(0, 0, 0, 0))

chordDiagram(df[, c(2, 4)], order = names(brand), grid.col = model_color,
             directional = 1, annotationTrack = "grid", preAllocateTracks = list(
               list(track.height = 0.02))
)

circos.track(track.index = 2, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.index = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), mean(ylim), sector.index, col = "white", cex = 0.6, facing = "inside", niceFacing = TRUE)
}, bg.border = NA)

for(b in unique(brand)) {
  model = names(brand[brand == b])
  highlight.sector(sector.index = model, track.index = 1, col = brand_color[b], 
                   text = b, text.vjust = -1, niceFacing = TRUE)
}

circos.clear()






# Read comparison data (only refresh from Google Sheets if changed) and save
comparison_data <- read_data_GD_comparison()
save(comparison_data,file="Data/Threddit-comparison_data.Rda")
# Load comparison data from file
load("Data/Threddit-comparison_data.Rda")

# Process raw comparison data to fit standard plot
comparison_data <- comparison_data %>% mutate(Category = as.factor(Category), comparison = TRUE) %>%
  select(-Details, -'Yearly cost', -Type, -Item, -'Cost per use') %>%
  as.data.frame()
colnames(comparison_data)<- c("category", "photo", "monthly_cost", "category_use", "comparison")

# Reduce master use data to latest date available
usetodate_comparison <- usetodate_anim %>% filter(date == max(usetodate_anim$date)) %>%
  mutate(monthly_cost = yearly_cost / 12, comparison = FALSE) %>%
  select(category, category_use, monthly_cost, photo, comparison, date)

# Combine category and comparison data sets for plotting
comparison_plot_data <- rbind.fill(usetodate_comparison, comparison_data) %>% mutate(date = max(usetodate_anim$date))



# Setup plot of columns $category_use and $monthly_cost
p <- setup_monthly_cost_and_category_use_plot(comparison_plot_data, ymax = 65, ybreaks = 5)

ggsave(filename = "Plots/Portfolio-Monthly_cost_and_Category_use-image.png", p, width = 12, height = 10, dpi = 300, units = "in")
file.copy("Plots/Portfolio-Monthly_cost_and_Category_use-image.png", "Website/Plots/Portfolio-Monthly_cost_and_Category_use-image.png", overwrite = TRUE)
#file.remove("Plots/Portfolio-Inventory-Item_count.png")




# Funtion to set up monthly cost vs category use plot
setup_monthly_cost_and_category_use_plot <- function(usetodate_comparison, ymax = NA, ybreaks = 2, animate = FALSE){
  
  # Animation marker size
  marker_size <- 40
  
  # Use only last date for image plots
  if(!animate) {
    usetodate_comparison <- usetodate_comparison %>% filter(date == max(usetodate_comparison$date))
    marker_size <- 28 # Set still marker size
  }
  
  # Set author label coordinates (upper right corner)
  author_label_x <- 1.0
  if(is.na(ymax)) { author_label_y = max(usetodate_comparison$yearly_cost) }
  else { author_label_y <- ymax }
  
  # Setup plot
  p <- usetodate_comparison %>%
    ggplot(aes(x = category_use, y = monthly_cost)) +
    annotate("text", x = author_label_x, y = author_label_y, label = author_label, color = "gray", hjust = 1) +
    geom_point(data = usetodate_comparison[usetodate_comparison$comparison ==  FALSE,], aes(colour = category), size = marker_size) +
    geom_point(data = usetodate_comparison[usetodate_comparison$comparison ==  TRUE,], colour = "lightgray", size = marker_size) +
    geom_image(aes(image = photo, group = date), size = 0.08) +
    scale_x_continuous(limits=c(0,1), labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(limits=c(NA,ymax), breaks=seq(0, 100, by = ybreaks)) +
    scale_color_manual(name = "Category", values = category_colors) +
    scale_size(range = c(1, 10)) +
    guides(size = FALSE) +
    labs(x = "Frequency of use (share of days used)", y = "Monthly cost (€ / month)") +
    guides(colour = guide_legend(override.aes = list(size = 2))) # Override plot point size to smaller for legend
  
  if(animate) {
    p <- p +
      transition_states(date, state_length = 1, transition_length = 0) +
      labs(title = "Date: {closest_state}") + ease_aes('linear')
  }
  
  return(p)
}








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
#install.packages("fitbitr")
library(fitbitr)

# Source required files
source("Fitbit-API-Key.R")

# Set Fitbit key and secret
FITBIT_KEY    <- get_fitbit_key()
FITBIT_SECRET <- get_fitbit_secret()
FITBIT_CALLBACK <- "http://localhost:1410/" 

# Authenticate and get token
#token <- fitbitr::oauth_token()
token <- generate_token(FITBIT_KEY, FITBIT_SECRET, FITBIT_CALLBACK)

# Set date of latest data
date <- "2022-01-22"

steps_2021 <- fitbitr::steps(start_date=as.Date("2018-01-01"), end_date = date)

# Get daily step data for entire item data period and remove duplicates
steps_2022 <- get_activity_time_series(token, "steps", date=date, period="1y")
steps_2021 <- get_activity_time_series(token, "steps", date="2021-12-31", period="1y")
steps_2020 <- get_activity_time_series(token, "steps", date="2020-12-31", period="1y")
steps_2019 <- get_activity_time_series(token, "steps", date="2019-12-31", period="1y")
steps_2018 <- get_activity_time_series(token, "steps", date="2018-12-31", period="1y")
steps <- rbind(steps_2021, steps_2020, steps_2019, steps_2018)
steps <- steps[!duplicated(steps$dateTime),]

# Remove temporary variables
rm(steps_2021)
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

# Save steps data.frame to file for easier retrieval (refreshed until 2021-05-23)
save(steps,file="Data/Threddit-steps.Rda")

# Load 'steps' variable from file
load("Data/Threddit-steps.Rda")

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
shoe_use <- shoe_use %>% mutate(cumsteps = cumsteps + cumsteps_init) %>% select(-cumsteps_init)



# Print total steps by item
shoe_use %>% group_by(item) %>%
  filter(cumsteps == max(cumsteps) & date == max(date)) %>%
  arrange(desc(cumsteps))

# Build and save plot
p <- shoe_use %>% filter(date == max(date)) %>% arrange(desc(active), cumsteps) %>% setup_shoes_steps_plot()
ggsave(filename = "Plots/Category-Shoes-Total_steps.png", p, width = 10, height = 10, dpi = 300, units = "in")
save_to_cloud("Category-Shoes-Total_steps.png")

