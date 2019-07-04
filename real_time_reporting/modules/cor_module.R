# Module UI function

cor_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Correlation between age and the expectance of automated data collection in years",
      background = "light-blue",
      tableOutput(ns("cor")))
  
}

# Module server function

cor_module <- function(input, output, session, data){
  
  output$cor <- renderTable(colnames = F, {
    
    cor_test <- cor.test(data()$q0,
                         data()$q2a,
                         method = "spearman")
    
    n_answers <- data() %>% count()
    
    tibble(c("Spearman rho correlation coefficient:", round(cor_test$estimate, 2)),
           c("Sample size:", n_answers))
    
  })
  
}

# Module util functions

