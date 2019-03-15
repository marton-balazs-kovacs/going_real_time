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
  
  url <- read_lines("gs_url.txt")
  
  ss <- gs_url(url, visibility = "public")
  
  refresh_time = 10000
  
  form_data <- reactiveValues()
  
  read_data <- reactive({
    
    invalidateLater(refresh_time)

    temp <- gs_read(ss) %>% 
      rename_all(funs(beg2char(., char = ".")))
    
    temp
  })
  
  observeEvent({!identical(form_data,read_data())},{
    
    form_data <- read_data()
  
  output$scatter_plot <- renderPlot({
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
  
  output$bar_plot <- renderPlot({
    form_data %>% 
      group_by(Q1) %>% 
      summarise(mean = round(mean(Q0, na.rm = T), 2),
                sd = round(sd(Q0, na.rm = T), 2),
                n = n(),
                se = sd / sqrt(n)) %>% 
      ggplot() +
      aes(x = Q1, y = mean) +
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

