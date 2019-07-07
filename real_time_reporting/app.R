# Loading packages

library(shiny)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))
library(BayesFactor)
library(papaja)

# Call modules

source("modules/scatter_module.R")
source("modules/bar_module.R")
source("modules/cor_module.R")
source("modules/bayes_plot_module.R")
source("modules/notification_module.R")
source("modules/connect_ss_module.R")
source("modules/generate_preprint_module.R")

# Define UI parts
## Define header

header <- dashboardHeader(title = "Real Time Data Analysis",
                          notification_module_output("bf_warning"))

## Define sidebar

sidebar <- dashboardSidebar(id = "", sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
  ))

## Define app body

body <- dashboardBody(
  tabItems(
    
    # First tab content
    
    tabItem(tabName = "dashboard",
            fluidRow(
              column(12, align="right",
                     bayes_plot_module_output("bayes"))),
            fluidRow(
              column(12, align="center",
                     scatter_module_output("scatter")
              )),
            fluidRow(
              column(12, align="center",
                     bar_module_output("bar")
              )),
            fluidRow(
              column(12, align="center",
                     cor_module_output("cor")
              ))
            )
    )
  )

# Define UI with combining UI parts

ui <- dashboardPage(header,
                    sidebar,
                    body)

# Define server logic

server <- function(input, output) {

  # Read in the url of the google spreadsheet that contains the output of your google form
  
  url <- read_lines("gs_url.txt")
  
  # Create connection through the url with the spreadsheet
  
  ss <- suppressMessages(gs_url(url, visibility = "public"))
  
  # Set the time interval for the refresh of the app (in milliseconds)
  
  refresh_time <- 10000
  
  # Create an empty reactive value for storing the output values
  
  values <- reactiveValues()
  
  # Set the default trigger value
  
  values$trigger <- NULL
  
  # Create an empty dataframe for storing the data
  
  values$form_data <- tibble()
  
  # Create a dataframe for storing the results of the Bayes factor calculation
  
  values$bf_data <- tibble(BF = numeric(0),
                           n_participant = integer(0))
  
  # Create a reactive expression for reading in the data from the spreadsheet
  
  read_data <- reactive({
    
    # Run the reactive expression after the time specified in [refresh_time]
    invalidateLater(refresh_time)
    
    # Read in data and transform variable names
    temp <- suppressMessages(gs_read(ss)) %>% 
      rename_all(~ str_to_lower(.)) %>% # Convert variable names to lower case  
      rename_at(vars(matches("[.]")),
                ~ str_extract(., "^([^.])+(?=\\.)")) # Save the identifiers of the question as variable names
  
    temp
  })
  
  # Create an expression that triggers the analysis if there are new responses to the form
  
  observe({
    
    invalidateLater(refresh_time)
    
    if(!identical(values$form_data, read_data())){
      
      values$trigger <-1
      values$form_data <- read_data()
      
    }else{
      
      values$trigger <- NULL}
  })
  
  # Create an expression for plotting the results of the analysis that is triggered by new responses
  
  observeEvent(values$trigger,{
    
    # Create a scatterplot 
    callModule(scatter_module, "scatter", data = reactive(values$form_data))
    
    # Create a table that shows correlation between variables
    callModule(cor_module, "cor", data = reactive(values$form_data))
    
    # Create a barplot
    callModule(bar_module, "bar", data = reactive(values$form_data))
    
  })
  
  # Create An expression for the Baye factor analysis
  observeEvent(values$trigger,{
    
    # Save data for the BF analysis
    bf_temp <- values$form_data %>% 
      filter(!is.na(q2a)) %>% 
      mutate(q1 = as.factor(q1))
    
    # Count the number of observations
    bf_temp_n <- bf_temp %>%
      count() %>% 
      pull(n)
    
    # Set the minimum number of participants required for the analyis
    bf_min_participant <- 15
    
    
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
    
    # Plot Bayes factors
    
    callModule(bayes_plot_module, "bayes", data = reactive(values$bf_data), bf_min_participant = bf_min_participant)
    
    }else{
      
      output$bf <- NULL
      
      # Create a notification icon if the sample size is not enough for BF analysis
      
      callModule(notification_module, "bf_warning")

      }
  })
  
  # Generate and download report
  
}

# Run the application 
shinyApp(ui = ui, server = server)