
# mod server
mod_pbServer <- function(id) {
  moduleServer(id, function(input, output, session) {


  #?
  ns <- session$ns  
      

  observeEvent(input$get_report,{

  al <- 
      action_levels(
        warn_at = req(input$warn_val)
      )

    agent <- 
      create_agent(
        tbl = ~ small_table,
        tbl_name = "small_table",
        label = "`export_report()`",
        actions = al
      )


    agent <-
      agent %>% 
      col_vals_gt(columns = d, value = 100) %>%
      col_vals_lte(columns = c, value = 5) %>%
      interrogate()

    #?
    export_report(
      agent,
      filename = "./www/report.html"
    ) 

    }) 


      
      
      #? show report
      output$rep_html <- renderUI({
      
      req(input$get_report)

      tags$iframe(seamless="seamless", 
                  src= "report.html",
                  width=800, 
                  height=800)
     })


  # end

  })
}