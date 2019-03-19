library(shiny)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))
library(BayesFactor)
library(shinyjs)

# Define UI parts
header <- dashboardHeader(title = "ISE 2019",
                          dropdownMenuOutput("bf_warning"))

sidebar <- dashboardSidebar(id = "", sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Bayes factor", tabName = "bayesfactor", icon = icon("th"))))

body <- dashboardBody(
  tabItems(
    
    # First tab content
    tabItem(tabName = "dashboard",
            fluidRow(
              box(title = "Relationship between age and the expectance of automated data collection",
                  status = "primary",
                  solidHeader = T,
                  plotOutput("scatter_plot",
                             height = 250)),
              
              box(title = "Correlation between age and the expectance of automated data collection in years",
                  background = "light-blue",
                  tableOutput("cor")),
              
              box(title = "Relationship between belief in automated research and age",
                  solidHeader = T,
                  plotOutput("bar_plot",
                             height = 250))
            )
    ),
    tabItem(tabName = "bayesfactor",
            icon = icon("cog", lib = "glyphicon"),
            fluidRow(
              box(title = "Bayes factor ábra",
                  solidHeader = T,
                  plotOutput("bf",
                             height = 250))
            )
    )
    ))

# Define UI
ui <- dashboardPage(header,
                    sidebar,
                    body)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  url <- "https://docs.google.com/spreadsheets/d/1ZL55BCrqqNNmSWr01g5ibL1yLNc-tfuk5IbKYlrnZu4/edit?usp=sharing"
  
  ss <- gs_url(url, visibility = "public")
  
  refresh_time = 10000
  
  values <- reactiveValues()
  values$trigger <- NULL
  values$form_data <- tibble()
  values$bf_data <- tibble(BF = numeric(0),
                           n_participant = integer(0))
  
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
        labs(y = "Age",
             x = "Expected time of automatizing data collection") +
        theme_minimal()
    }) 
    
    output$cor <- renderTable(colnames = F, {
      cor_test <- cor.test(values$form_data$q0, values$form_data$q2a, method = "spearman")
      n_answers <- values$form_data %>% count()
      
      tibble(c("Spearman rho correlation coefficient:", cor_test$estimate),
             c("Sample size:", n_answers))
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
        labs(x = "I will be possible to fully automatize the research process",
             y = "Age") +
        theme_minimal()})
    
  })
  
  observeEvent(values$trigger,{
    
    bf_temp <- values$form_data %>% 
      filter(!is.na(q2a)) %>% 
      mutate(q1 = as.factor(q1))
    
    bf_temp_n <- bf_temp %>%
      count() %>% 
      pull(n)
    
    bf_min_participant = 15
    
    if(bf_temp_n > bf_min_participant){
      output$bf_warning <- NULL
      if(nrow(values$bf_data) == 0){
        
        for(i in bf_min_participant:bf_temp_n){
          bf_temp_sub <- bf_temp[1:i,]
          
          new_line <- tibble(BF = as.numeric(as.vector(ttestBF(formula =  q2a ~ q1, data = bf_temp_sub))),
                             n_participant = as.numeric(i))
          
          values$bf_data <- bind_rows(values$bf_data, new_line)}
        
        }else{
        new_participant_n <- max(values$bf_data$n_participant)
        
        for(i in new_participant_n:bf_temp_n){
          bf_temp_sub <- bf_temp[1:i,]
          new_line <- tibble(BF = as.numeric(as.vector(ttestBF(formula =  q2a ~ q1, data = bf_temp_sub))),
                             n_participant = as.numeric(i))
          values$bf_data <- bind_rows(values$bf_data, new_line)}
        }
      

    
    # Plot
    x_limit_max <- max(values$bf_data$n_participant) * 1.5
    
    output$bf <- renderPlot({
      values$bf_data %>% 
        ggplot() +
        aes(x = n_participant, y = as.numeric(BF)) +
        geom_point() +
        geom_line() +
        labs(y = "log(BF)", x = "Number of participants") +
        scale_y_continuous(breaks = c(c(-log(c(30, 10, 3)), 0, log(c(3, 10, 30)))),
                           labels = c("-log(30)", "-log(10)", "-log(3)", "log(1)", "log(3)", "log(10)", "log(30)")) +
        scale_x_continuous(limits = c(bf_min_participant, x_limit_max)) +
        coord_cartesian(ylim=c(-log(40),log(40))) +
        theme_minimal() +
        geom_hline(yintercept=c(c(-log(c(30, 10, 3)), log(c(3, 10, 30)))), linetype="dotted", color="darkgrey") +
        geom_hline(yintercept=log(1), linetype="dashed", color="darkgreen") +
        geom_hline(yintercept=log(3), linetype="dashed", color="red") +
        geom_hline(yintercept=-log(3), linetype="dashed", color="red") +
        geom_point(data = values$bf_data[nrow(values$bf_data),], aes(x=n_participant, y=as.numeric(BF)), color="red", size=2) +
        annotate("text", x=x_limit_max, y=-2.85, label = "StrongH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
        annotate("text", x=x_limit_max, y=-1.7 , label = "ModerateH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
        annotate("text", x=x_limit_max, y=-.55 , label = "AnectodalH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
        annotate("text", x=x_limit_max, y=2.86 , label = "StrongH1", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
        annotate("text", x=x_limit_max, y=1.7  , label = "ModerateH1", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
        annotate("text", x=x_limit_max, y=.55  , label = "AnectodalH1", hjust=1, vjust=.5, vjust=.5, size=3, color="black", parse=TRUE)
      })
    
    }else{
      output$bf <- NULL
      
    output$bf_warning <- renderMenu({
      dropdownMenu(type = "notifications", badgeStatus = "warning",
                   notificationItem(text = "Sample size is not enough for BF analysis.",
                                    icon = icon("ok", lib = "glyphicon"),
                                    status = "danger"))
  })
}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)