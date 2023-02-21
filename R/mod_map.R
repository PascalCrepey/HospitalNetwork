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
    div(id = ns("hideUI_net"),
        "The network has not been constructed yet"),
    div(id = ns("hideUI_gps"),
        "You did not provide GPS coordinates"),
    div(id = ns("showUI"),
        fluidPage(fluidRow(
          column(9,leafletOutput(ns("mymap"),
                                 height = 800)),
          column(3,uiOutput(ns("min_flow_UI"))))
        ))
        
  )
}

#' map Server Functions
#'
#' @rdname mod_map
#' @export
#' @keywords internal
#' @import leaflet
#' @importFrom leaflet.extras2 addArrowhead
#' @importFrom geosphere gcIntermediate
mod_map_server <- function(input, output, session, net){
    ns <- session$ns

    observe({
      shinyjs::toggle("hideUI_net", condition = is.null(net()))
      shinyjs::toggle("hideUI_gps", condition = (!is.null(net()) & 
                                                   all(is.na(net()$facilities$lat))))
      shinyjs::toggle("showUI", condition = (!is.null(net()) & 
                                               any(!is.na(net()$facilities$lat))))
    })

    output$mymap = renderLeaflet({
      data_flow = merge(net()$edgelist, 
                         net()$facilities, 
                         by.x = "origin", by.y = "node")
      data_flow = merge(data_flow, 
                         net()$facilities, 
                         by.x = "target", by.y = "node",
                         suffixes = c("_origin", "_target"))
      req(input$min_flow)
      data_flow = data_flow[N >= input$min_flow,]
      
      flows <- gcIntermediate(data_flow[, .(long_origin, lat_origin)], 
                              data_flow[, .(long_target, lat_target)], 
                              sp = TRUE, addStartEnd = TRUE)
      flows$counts <- data_flow$N
      flows$origins <- data_flow$origin
      flows$destinations <- data_flow$target
      
      leaflet(data_flow) %>%
        addTiles() %>%
        addCircleMarkers(data = net()$facilities,
                         lng = ~long,
                         lat = ~lat,
                         radius = ~beds/100,
                         popup = paste("Facility ", net()$facilities$node, 
                                       " (n=", net()$facilities$beds, ")", sep = ""),
                         color = "#20B2AA",
                         fillOpacity = 0.1) %>%
        addArrowhead(data = flows,
                     weight = 1,
                     label = paste("From ", flows$origins, " to ", flows$destinations, " (n=", 
                                   flows$counts, ")", sep = ""),
                     opacity = 0.5, 
                     options = arrowheadOptions(yawn = 40, frequency = "endonly", 
                                                fill = TRUE))
    })
    
    # UI for minimal flow ----
    output$min_flow_UI <- renderUI({
      sliderInput(ns("min_flow"), "Minimal flow to be shown", 
                  min = 1, 
                  max = max(net()$edgelist$N), 
                  step = 1,
                  value = pmax(1, max(net()$edgelist$N) - 2))
    })
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# mod_map_server("map_1")
