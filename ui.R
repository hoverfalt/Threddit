#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    titlePanel("Wardrobe Performance"),
    sidebarLayout(
        sidebarPanel(
            h4("Plot type"),
            radioButtons("plot_type", "Plot type:",
                         c("Image" = "Image",
                           "Point" = "Point")),
            h4("Categories to include"),
            checkboxInput("Jackets_and_hoodies", "Jackets and hoodies", value = TRUE, width = NULL),
            checkboxInput("Blazers_and_vests", "Blazers and vests", value = FALSE, width = NULL),
            checkboxInput("Knits", "Knits", value = FALSE, width = NULL),
            checkboxInput("Shirts", "Shirts", value = FALSE, width = NULL),
            checkboxInput("T-shirts_and_tanks", "T-shirts and tanks", value = FALSE, width = NULL),
            checkboxInput("Pants", "Pants", value = FALSE, width = NULL),
            checkboxInput("Shorts", "Shorts", value = FALSE, width = NULL),
            checkboxInput("Belts", "Belts", value = FALSE, width = NULL),
            checkboxInput("Socks", "Socks", value = FALSE, width = NULL),
            checkboxInput("Shoes", "Shoes", value = FALSE, width = NULL),
            checkboxInput("Underwear_shirts", "Underwear shirts", value = FALSE, width = NULL),
            checkboxInput("Underwear_boxers", "Underwear boxers", value = FALSE, width = NULL),
            checkboxInput("Sportswear", "Sportswear", value = FALSE, width = NULL),
            h4("Plot area limits"),
            sliderInput("xmax", "Average times worn per month", 0, 15, value = 4),
            sliderInput("ymax", "Cost per wear", 0, 200, value = 10),
            width = 3
        ),
        mainPanel(
            plotOutput("plot"), 
        )
    )
))