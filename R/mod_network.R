# Module UI
  
#' @title   mod_network_ui and mod_network_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_network
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_network_ui <- function(id){
  ns <- NS(id)
  tagList(
      div(id = ns("hideUI"),
          "The network has not been constructed yet"),
      div(id = ns("showUI"),
          fluidRow(
              column(5,
                     tabBox(
                         title = HTML("<b>Edgelist</b>"),
                         id = ns("edgelist"),
                         tabPanel("aggregated",
                                  DT::DTOutput(ns("el_aggr"))),
                         tabPanel("long",
                                  DT::DTOutput(ns("el_long"))),
                         side = "right",
                         width = NULL)),
              column(7,
                     tabBox(
                         title = HTML("<b>Plots</b>"),
                         id = ns("plots"),
                         tabPanel("matrix",
                                  plotOutput(ns("plot_matrix"))),
                         tabPanel("clustered matrix",
                                  plotOutput(ns("plot_clustered_matrix"))),
                         side = "right",
                         width = NULL))
          ))
  )
}
    
# Module Server
    
#' @rdname mod_network
#' @export
#' @keywords internal
    
mod_network_server <- function(input, output, session, net){
    ns <- session$ns

    observe({
        shinyjs::toggle("hideUI", condition = is.null(net()))
        shinyjs::toggle("showUI", condition = !is.null(net()))
    })

    output$el_aggr = DT::renderDT({ net()$edgelist },
                                  filter = "bottom",
                                  options = list(scrollY = "400px",
                                                 scrollX = "200px"))
    output$el_long = DT::renderDT({ net()$edgelist_long },
                                  filter = "bottom",
                                  options = list(scrollY = "400px",
                                                 scrollX = "200px"))
    output$plot_matrix = renderPlot({ net()$plot("matrix") })
    output$plot_clustered_matrix = renderPlot({ net()$plot("clustered_matrix") })

    
}
    
## To be copied in the UI
# mod_network_ui("network_ui_1")
    
## To be copied in the server
# callModule(mod_network_server, "network_ui_1")
 
