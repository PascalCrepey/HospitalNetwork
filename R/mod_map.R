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

    # Join the two datasets using the "origin" and "target" variables
    data_flow <- merge(net()$edgelist, net()$facilities, by.x = "origin", by.y = "beds")
    data_flow <- merge(data_flow, net()$facilities, by.x = "target", by.y = "beds",
                       suffixes = c("_origin", "_target"

    # output$mymap <- renderLeaflet({
    #   leaflet(net()$facilities) %>%
    #     addTiles() %>%
    #     addCircleMarkers(lng = ~long,
    #                      lat = ~lat,
    #                      radius = ~beds/1000,
    #                      popup = paste(#"Nom de l'hôpital:", hospital_name, "<br>",
    #                                    "Nombre de lits:", ~beds),
    #                      color = "#20B2AA",
    #                      fillOpacity = 0.7) #%>%
    # })

    output$mymap <- renderLeaflet({
      leaflet(data_flow) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~longitude_origin,
                         lat = ~latitude_origin,
                         radius = ~beds_origin/1000,
                         popup = paste("Nombre de lits (origine):", ~beds_origin),
                         color = "#20B2AA",
                         fillOpacity = 0.7) %>%
        addArrows(lng0 = ~longitude_origin,
                  lat0 = ~latitude_origin,
                  lng1 = ~longitude_target,
                  lat1 = ~latitude_target,
                  arrowLength = 5,
                  color = "blue",
                  weight = 2)
    })
  })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
