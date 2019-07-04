# Module UI function

notification_module_output <- function(id){
  
  ns <- NS(id)
  
  dropdownMenuOutput("notification")
  
}

# Module server function

notification_module <- function(input, output, session){
  
  output$notification <- renderMenu({
    
    dropdownMenu(type = "notifications", badgeStatus = "warning",
                 notificationItem(text = "Sample size is not enough for BF analysis.",
                                  icon = icon("ok", lib = "glyphicon"),
                                  status = "danger"))
    
  })
}

# Module util functions

# There are no util functions for this module.