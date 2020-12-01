### Threddit.R - Olof Hoverfält - 2020 ###

### Functions to build plots



### STANDARD PORTFOLIO PLOTS #################################################################################################

build_standard_portfolio_plots <- function(){
  
  # Active inventory item count by category
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_item_count_plot()
  ggsave(filename = "Website/Plots/Portfolio-Inventory-Item_count.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')

  # Active inventory value by category (line plot)
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_by_category_plot()
  ggsave(filename = "Website/Plots/Portfolio-Inventory-Value_by_category.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')

  # Active inventory value by category (stacked area plot)
  p <- inventory %>% filter(category != "Sportswear") %>% setup_inventory_value_stacked_plot()
  ggsave(filename = "Website/Plots/Portfolio-Inventory-Value_stacked.png", p, width = 12, height = 10, dpi = 300, units = "in", device='png')
}





