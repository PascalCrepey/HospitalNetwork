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
          leafletOutput(ns("mymap")),
          uiOutput(ns("min_flow_UI")))
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

    output$mymap = renderLeaflet({
      data_flow = merge(net()$edgelist, 
                         net()$facilities, 
                         by.x = "origin", by.y = "node")
      data_flow = merge(data_flow, 
                         net()$facilities, 
                         by.x = "target", by.y = "node",
                         suffixes = c("_origin", "_target"))
      
      leaflet(data_flow) %>%
        addTiles() %>%
        addCircleMarkers(lng = ~long_origin,
                         lat = ~lat_origin,
                         radius = ~beds_origin/1000,
                         popup = paste("Capacity = ", data_flow$beds_origin, " beds"),
                         color = "#20B2AA",
                         fillOpacity = 0.7) %>%
        addFlows(lng0 = data_flow[N >= input$min_flow, long_origin],
                 lat0 = data_flow[N >= input$min_flow, lat_origin],
                 lng1 = data_flow[N >= input$min_flow, long_target],
                 lat1 = data_flow[N >= input$min_flow, lat_target],
                 flow = data_flow[N >= input$min_flow, N],
                 color = "blue",
                 maxThickness = 1,
                 opacity = 0.3)
    })
    
    # UI for minimal flow ----
    output$min_flow_UI <- renderUI({
      sliderInput(ns("min_flow"), "Minimal flow to be shown", 
                  min = 1, 
                  max = max(net()$edgelist$N), 
                  step = 1,
                  value = max(net()$edgelist$N))
    })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
