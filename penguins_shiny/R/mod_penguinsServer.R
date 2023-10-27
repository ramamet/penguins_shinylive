
# mod server
mod_penguinsServer <- function(id) {
  moduleServer(id, function(input, output, session) {


  #?
  ns <- session$ns  

  #? select species
  selectedSpecies <- reactive({
    penguins %>% 
    filter(!is.na(sex)) %>%
    dplyr::filter(species==input$species)
  })

  
  #? output
  output$scatterPlot <- renderPlot({
   # plot
   ggplot(selectedSpecies(),aes(x=bill_length_mm,y=bill_depth_mm))+
        geom_point(shape = 21, colour = "#775c6a", fill = "#f4828c", size = 3, stroke = 1, alpha = 0.7)+
        facet_grid(cols = vars(!!rlang::sym(input$facet_grp))) +
        labs(x ="Bill Length (mm)", y= "Bill Depth (mm)") +
        theme(aspect.ratio = 1)  +
        theme_bw()      

  })

  # echarts
  output$ec_plot1 <- renderEcharts4r({

     temp_df <- selectedSpecies() %>%                            
                group_by((!!rlang::sym(input$facet_grp))) %>%  
                summarize(n = n())

      names(temp_df) <- c('grp', 'n')

     # plot
      temp_df %>%
      mutate(grp = as.factor(as.character(grp))) %>%
      e_charts(x = grp) %>%         
      e_pie(n, 
        selectedOffset= 15,
        radius = c("45%", "75%"),
        itemStyle = list(borderRadius = 10,
                         borderColor= '#fff',
                         borderWidth= 2)) %>%
      e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                        return('<strong>' + params.name + 
                                        '</strong><br />total: ' + params.value + 
                                        '<br />percent: ' +  params.percent)  +'%' }  ")) %>%
      e_labels(show = FALSE) 
  })




  # end

  })
}