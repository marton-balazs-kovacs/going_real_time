library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))
library(DT)
library(qdap)

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
  
  url <- read_lines("gs_url.txt")
  
  ss <- gs_url(url, visibility = "public")
  
  refresh_time = 10000
  
  observe({
    
    invalidateLater(refresh_time)

    form_data <- gs_read(ss)
    
    form_data <- form_data %>% 
      rename_all(funs(beg2char(., char = ".")))
    
    output$plot <- renderPlot({
      form_data %>%
      ggplot() +
        aes(x = Q0,
            y = Q2a) +
        geom_point()
    }) 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

