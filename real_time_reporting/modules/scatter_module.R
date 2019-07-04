# Module UI function

scatter_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Relationship between age and the expectance of automated data collection",
      status = "primary",
      solidHeader = T,
      plotOutput(ns("plot"),
                 height = 250))
  
}

# Module server function

scatter_module <- function(input, output, session, data){
  
    output$plot <- renderPlot({
      
      scatter_plot(data = data())
      
      })
    }

# Module util functions

scatter_plot <- function(data){
  
  data %>%
    ggplot() +
    aes(x = q0,
        y = q2a) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(y = "Age",
         x = "Expected time of automatizing data collection") +
    theme_minimal()
  
}