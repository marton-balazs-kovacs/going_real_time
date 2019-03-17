library(shiny)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      box(title = "Automatizált adatgyűjtés és életkor közti összefüggés",
          status = "primary",
          solidHeader = T,
          plotOutput("scatter_plot",
                     height = 250)),
      
      box(title = "Életkor és adatgyűjtés közti korreláció",
          background = "light-blue",
          tableOutput("cor")),
      
      box(title = "Autamatizált tudományba vetett hit és kor közti összefüggés",
          solidHeader = T,
          plotOutput("bar_plot",
                     height = 250))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  url <- "https://docs.google.com/spreadsheets/d/1ZL55BCrqqNNmSWr01g5ibL1yLNc-tfuk5IbKYlrnZu4/edit?usp=sharing"
  
  ss <- gs_url(url, visibility = "public")
  
  refresh_time = 10000
  
  values <- reactiveValues()
  values$trigger <- NULL
  values$form_data <- tibble()
  
  read_data <- reactive({
    
    invalidateLater(refresh_time)
    
    temp <- gs_read(ss) %>% 
      rename_all(~ str_to_lower(.)) %>% 
      rename_at(vars(matches("[.]")), ~ str_extract_all(., "^([^.])+(?=\\.)"))
    
    temp
  })
  
  observe({
    
    invalidateLater(refresh_time)
    
    if(! identical(values$form_data, read_data())){
      values$trigger <-1
      values$form_data <- read_data()
    }else{values$trigger <- NULL}
  })
  
  observeEvent(values$trigger,{
    
    output$scatter_plot <- renderPlot({
      values$form_data %>%
        ggplot() +
        aes(x = q2a,
            y = q0) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(y = "Életkor",
             x = "Adatgyűjtés automatizálásának várható ideje") +
        theme_minimal()
    }) 
    
    output$cor <- renderTable(colnames = F, {
      cor_test <- cor.test(values$form_data$q0, values$form_data$q2a, method = "spearman")
      n_answers <- values$form_data %>% count()
      
      tibble(c("Spearman rho korrelációs együttható:", cor_test$estimate),
             c("Megfigyelések száma:", n_answers))
    })
    
    output$bar_plot <- renderPlot({
      values$form_data %>% 
        group_by(q1) %>% 
        summarise(mean = round(mean(q0, na.rm = T), 2),
                  sd = round(sd(q0, na.rm = T), 2),
                  n = n(),
                  se = sd / sqrt(n)) %>% 
        ggplot() +
        aes(x = q1, y = mean) +
        geom_bar(stat = "identity") +
        geom_errorbar(aes(ymin = mean - se,
                          ymax = mean + se),
                      position = "dodge") +
        labs(x = "Lehetséges lesz automatizálni a tudományos folyamatot?",
             y = "Kor") +
        theme_minimal()})
  })
}

# Run the application 
shinyApp(ui = ui, server = server)