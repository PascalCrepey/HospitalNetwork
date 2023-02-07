#' map UI Function
#'
#' @description A shiny Module to create the map of the network using leaflet.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    leafletOutput("mymap")
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    data_tmp = dataset_europe[country == "Italy",]
    
    output$mymap <- renderLeaflet({
      leaflet(data_tmp) %>%
        addTiles() %>% 
        addCircleMarkers(lng = ~long,
                         lat = ~lat,
                         radius = ~beds_real_number/1000)
    })
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
