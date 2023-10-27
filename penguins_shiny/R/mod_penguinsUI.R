# mod ui
mod_penguinsUI <- function(id) {

  ns <- NS(id)


  tagList(

  br(), 
  h2("Palmer Penguins Data Exploration"),
  br(),

  #? fluidrows
  fluidRow(
  
      column(6,
      wellPanel(
      selectInput(ns("species"), 'Species', unique(penguins$species), selected="Adelie")
      )),

      column(6,
      wellPanel(
      selectInput(ns("facet_grp"), 'Group-by', c('island', 'year', 'sex'), selected="island")
      ))
  )

  , fluidRow(
      column(6,
      echarts4rOutput(ns("ec_plot1"))
      )
     , column(6,
      plotOutput(ns("scatterPlot"))
      )
  )

# end  

)
}