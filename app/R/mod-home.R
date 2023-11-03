homeUI <- function(id) {
  ns <- NS(id)

  tagList(

    #mod_pbUI("id1")
     load_dataUI(ns('mod_df')),
     br(),
     DT::dataTableOutput(ns("mytable"))


  )

}


#?
 homeServer <- function(id) {
  moduleServer(id,
    function(input, output, session) {

    
    
    load_df <- load_dataServer('mod_df')

    output$mytable = DT::renderDataTable({
    load_df() %>% head()
    })
    
    
    
    
    
    
    }
  )}