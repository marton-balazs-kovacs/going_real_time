scatter_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Relationship between age and the expectance of automated data collection",
      status = "primary",
      solidHeader = T,
      plotOutput(ns("plot"),
                 height = 250))
  
}

scatter_module <- function(input, output, session, data){
  
  data <- reactive(data())
  
    output$scatter_plot <- renderPlot({
      
      data %>%
        ggplot() +
        aes(x = q1,
            y = q2a) +
        geom_point() +
        geom_smooth(method = "lm") +
        labs(y = "Age",
             x = "Expected time of automatizing data collection") +
        theme_minimal()
      })
}