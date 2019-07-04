# Module UI function

bayes_plot_module_output <- function(id){
  
  ns <- NS(id)
  
  box(title = "Bayes factor plot",
      solidHeader = T,
      plotOutput(ns("plot")))
  
}

# Module server function

bayes_plot_module <- function(input, output, session, data, bf_min_participant){
  
    output$plot <- renderPlot({
      
      bayes_plot(data = data(),
                 bf_min_participant = bf_min_participant)
      
      })
    }

# Module util functions

bayes_plot <- function(data, bf_min_participant){
  
  # Set parameters of plotting area
  
  x_limit_max <- max(data$n_participant) * 1.5
  
  y_limit_max <- if_else(max(data$BF) > log(40),
                         max(data$BF),
                         log(40))
  
  y_limit_min <- if_else(min(data$BF) < -log(40),
                         min(data$BF),
                         -log(40))
  
  # Create the plot
  
  data %>% 
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
    geom_point(data = data[nrow(data),], aes(x=n_participant, y=as.numeric(BF)), color="red", size=2) +
    annotate("text", x=x_limit_max, y=-2.85, label = "StrongH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
    annotate("text", x=x_limit_max, y=-1.7 , label = "ModerateH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
    annotate("text", x=x_limit_max, y=-.55 , label = "AnectodalH0", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
    annotate("text", x=x_limit_max, y=2.86 , label = "StrongH1", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
    annotate("text", x=x_limit_max, y=1.7  , label = "ModerateH1", hjust=1, vjust=.5, size=3, color="black", parse=TRUE) +
    annotate("text", x=x_limit_max, y=.55  , label = "AnectodalH1", hjust=1, vjust=.5, vjust=.5, size=3, color="black", parse=TRUE)
  
}