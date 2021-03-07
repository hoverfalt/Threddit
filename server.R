#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

#    plot_type <- reactive({ plot_type <- input$plot_type })
#    xmax <- reactive({ xmax <- input$xmax })
#    ymax <- reactive({ ymax <- input$ymax })

    output$plot <- renderPlot({
        plot_type <- input$plot_type
        xmax <- input$xmax
        ymax <- input$ymax
        
        # Initiate category boolean vector
        categories_mask <- rep(FALSE, length(category_order))
        
        # Populate category boolrean vector with input
        categories_mask[1] <- input$Jackets_and_hoodies
        categories_mask[2] <- input$Blazers_and_vests
        categories_mask[3] <- input$Knits
        categories_mask[4] <- input$Shirts
        categories_mask[5] <- input$'T-shirts_and_tanks'
        categories_mask[6] <- input$Pants
        categories_mask[7] <- input$Shorts
        categories_mask[8] <- input$Belts
        categories_mask[9] <- input$Socks
        categories_mask[10] <- input$Shoes
        categories_mask[11] <- input$Underwear_shirts
        categories_mask[12] <- input$Underwear_boxers
        categories_mask[13] <- input$Sportswear
        
        # Create list of categories
        categories_to_plot <- category_order[categories_mask]
        
        if(plot_type == "Image") { p <- setup_category_plot_image(plot_data, categories = categories_to_plot, xmax = xmax, ymax = ymax,
                                                                  ybreaks = plot_log_breaks, log_trans=TRUE, fixed_marker_size = 35) }
        if(plot_type == "Point") { p <- setup_category_plot_point(plot_data, categories = categories_to_plot, xmax = xmax, ymax = ymax,
                                                                  ybreaks = plot_log_breaks, log_trans=TRUE) }
        p
        
        #p <- plot_data %>% setup_category_cumulative_plot_image(categories = categories, xmax = xmax, ymax = ymax, ybreaks = plot_log_breaks, log_trans=TRUE, trails=TRUE, guides=TRUE)
        #p <- plot_data %>% setup_category_plot_image(categories = categories, xmax = xmax, ymax = ymax, ybreaks = plot_log_breaks, log_trans=TRUE)
        #p <- plot_data %>% setup_category_plot_point(categories = categories_to_plot, xmax = xmax, ymax = ymax, ybreaks = plot_log_breaks, log_trans=TRUE)
        
    }, height = 880, width = 880)
    
})


## Example ersion

