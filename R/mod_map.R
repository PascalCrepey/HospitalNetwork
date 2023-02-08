#' map UI Function
#'
#' @description A shiny Module to create the map of the network using leaflet.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
#' @import leaflet
mod_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("hideUI"),
        "The network has not been constructed yet"),
    div(id = ns("showUI"),
        fluidPage(
          leafletOutput(ns("mymap")))
        )
  )
}

#' map Server Functions
#'
#' @rdname mod_map
#' @export
#' @keywords internal
#' @import leaflet
#' @importFrom leaflet.minicharts addFlows
mod_map_server <- function(input, output, session, net){
    ns <- session$ns

    observe({
      shinyjs::toggle("hideUI", condition = is.null(net()))
      shinyjs::toggle("showUI", condition = !is.null(net()))
    })
    
    # # Join the two datasets using the "origin" and "target" variables
    # data_flow <- merge(net()$edgelist, net()$facilities, by.x = "origin", by.y = "beds")
    # data_flow <- merge(data_flow, net()$facilities, by.x = "target", by.y = "beds",
    #                    suffixes = c("_origin", "_target"))
    
    # output$mymap <- renderLeaflet({
    #   leaflet(dataset_europe[country == "France"]) %>%
    #     addTiles() %>%
    #     addCircleMarkers(lng = ~long,
    #                      lat = ~lat,
    #                      radius = ~beds/1000,
    #                      popup = paste(#"Nom de l'hôpital:", hospital_name, "<br>",
    #                                    "Nombre de lits:", ~beds),
    #                      color = "#20B2AA",
    #                      fillOpacity = 0.7)
    # })
    # 
    output$mymap <- renderLeaflet({
      data_flow <- merge(net()$edgelist, net()$facilities, by.x = "origin", by.y = "node")
      data_flow <- merge(data_flow, net()$facilities, by.x = "target", by.y = "node",
                         suffixes = c("_origin", "_target"))
      leaflet(data_flow) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~long_origin,
                         lat = ~lat_origin,
                         radius = ~beds_origin/1000,
                         popup = paste0("Capacity = ", ~beds_origin, " beds"),
                         color = "#20B2AA",
                         fillOpacity = 0.7) %>%
        addFlows(lng0 = data_flow$long_origin,
                 lat0 = data_flow$lat_origin,
                 lng1 = data_flow$long_target,
                 lat1 = data_flow$lat_target,
                 flow = data_flow$N,
                 color = "blue",
                 maxThickness = 1,
                 opacity = 0.4)
    })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
