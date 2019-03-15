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
      column(width = 12,
      box(title = "Automatizált adatgyűjtés és életkor közti összefüggés",
          status = "primary",
          solidHeader = T,
          plotOutput("plot", height = 250)),
      box(title = "Életkor és adatgyűjtés közti korreláció",
          background = "light-blue",
          tableOutput("cor"))
      )
  )
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
        aes(x = Q2a,
            y = Q0) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(y = "Életkor",
             x = "Adatgyűjtés automatizálásának várható ideje") +
        theme_minimal()
    }) 
    
    output$cor <- renderTable(colnames = F, {
      
      cor_test <- cor.test(form_data$Q0, form_data$Q2a, method = "spearman")
      
      tibble(c("Spearman rho korrelációs együttható:", cor_test$estimate),
             c("Megfigyelések száma:", cor_test$statistic))
    })
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

