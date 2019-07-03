# Module UI function

bar_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Relationship between belief in automated research and age",
      solidHeader = T,
      plotOutput(ns("plot"),
                 height = 250))
  
}

# Module server function

bar_module <- function(input, output, session, data){
  
    output$plot <- renderPlot({
      
      data() %>% 
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
        theme_minimal()
      
      })
}

# Module util functions