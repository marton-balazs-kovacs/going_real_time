# Module UI function

generate_preprint_module_ui <- function(id){
  
  ns <- NS(id)
  
  downloadButton(ns("preprint"), "Generate report")
  
}

# Module server function

generate_preprint_module <- function(input, output, session){
  
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

# Module util functions

