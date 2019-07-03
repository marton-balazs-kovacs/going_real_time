# Loading packages
library(shiny)
library(shinydashboard)
library(googlesheets)
suppressPackageStartupMessages(library(tidyverse))
library(BayesFactor)
library(papaja)

# Call modules
#source(".R")

# Define UI parts
## Define header
header <- dashboardHeader(title = "Real Time Data Analysis",
                          dropdownMenuOutput("bf_warning"))

## Define sidebar
sidebar <- dashboardSidebar(id = "", sidebarMenu(
  menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
  menuItem("Bayes factor", tabName = "bayesfactor", icon = icon("th"))))

## Define app body
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
            ),
            fluidRow(
              downloadButton("preprint", "Generate report")
            )
    ),
    
    # Second tab content
    tabItem(tabName = "bayesfactor",
            icon = icon("cog", lib = "glyphicon"),
            fluidRow(
              box(title = "Bayes factor plot",
                  solidHeader = T,
                  height = "auto",
                  width = "auto",
                  plotOutput("bf",
                             height = 300,
                             width = 600))
            )
    )
    ))

# Define UI with combining UI parts
ui <- dashboardPage(header,
                    sidebar,
                    body)

# Define server logic
server <- function(input, output) {
  
  # Read in the url of the google spreadsheet that contains the output of your google form
  url <- read_lines("gs_url.txt")
  
  # Create connection through the url with the spreadsheet
  ss <- gs_url(url, visibility = "public")
  
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
    temp <- gs_read(ss) %>% 
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
    
    # Create a table that shows correlation between variables
    output$cor <- renderTable(colnames = F, {
      cor_test <- cor.test(values$form_data$q0, values$form_data$q2a, method = "spearman")
      n_answers <- values$form_data %>% count()
      
      tibble(c("Spearman rho correlation coefficient:", cor_test$estimate),
             c("Sample size:", n_answers))
    })
    
    # Create a barplot
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
        labs(x = "I believe that it will be possible to fully automatize the research process",
             y = "Age") +
        theme_minimal()})
    
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
      

    
    # Create the plot for the BF analysis
    x_limit_max <- max(values$bf_data$n_participant) * 1.5
    
    y_limit_max <- if_else(max(values$bf_data$BF) > log(40),
                           max(values$bf_data$BF),
                           log(40))
    
    y_limit_min <- if_else(min(values$bf_data$BF) < -log(40),
                           min(values$bf_data$BF),
                           -log(40))
    
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
        coord_cartesian(ylim=c(y_limit_min, y_limit_max)) +
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
      
      # Create a notification icon if the sample size is not enough for BF analysis
      output$bf_warning <- renderMenu({
        dropdownMenu(type = "notifications", badgeStatus = "warning",
                     notificationItem(text = "Sample size is not enough for BF analysis.",
                                      icon = icon("ok", lib = "glyphicon"),
                                      status = "danger"))
  })
}
  })
  
  # Generate and download report
  output$preprint <- downloadHandler(
    
    filename = "preprint.html",
    
    content = function(file) {

      temp_preprint <- file.path(tempdir(), "preprint.Rmd")
      file.copy("preprint.Rmd", temp_preprint, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(scatter = output$scatter_plot)
      
      rmarkdown::render(input = temp_preprint,
                        output_format = "html_document",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)