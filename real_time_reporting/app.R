library(shiny)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))

url <- read_lines("real_time_reporting/gs_url.txt")

ss <- gs_url(url, visibility = "public")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(plotOutput("plot", height = 250)))
  )
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot <- renderPlot({
    data <- 
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

