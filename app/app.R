
library(shiny)
library(purrr)
library(dplyr)
library(DT)
library(htmltools)
library(shinyWidgets)
library(datamods)
library(pointblank)

# source external files



# source all modules
mod_file_path <- paste0('./R')

# only specific pattern in the directory
mod_scripts <- list.files(path= mod_file_path, pattern = glob2rx("mod_*.R")) %>% as_vector()

#
analyticscripts <- c(mod_scripts) %>% 
                   as_tibble() %>%
                   mutate(value = paste0(mod_file_path,'/',value))

# load all functions
purrr::walk(analyticscripts$value, source)

#? Shiny modules
# mod app
penguinsApp <- function() {
  ui <- fluidPage(
   homeUI("id1")
  )
  server <- function(input, output, session) {
    homeServer("id1")
  }
  shinyApp(ui, server)  
}


penguinsApp()