# mod ui
mod_pbUI <- function(id) {

  ns <- NS(id)


  tagList(

  br(), 
  h2("Pointblank"),
  br(),

   numericInput(ns("warn_val"), "Warn_at:", 0.6, min = 0.1, max = 1.0, step = 0.1),

   actionButton(ns("get_report"), "Report"),
   br(),
   htmlOutput(ns("rep_html")) 



# end  

)
}