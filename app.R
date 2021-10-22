#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(pkgstats)
library(purrr)
library(DT)
load("df3.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
    fluidRow(
     column(
         width = 12,
         DT::DTOutput("out")
     )   
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$out <- DT::renderDT(df3, server = TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
