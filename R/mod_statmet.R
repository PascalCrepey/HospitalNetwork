# Module UI
  
#' @title   mod_statmet_ui and mod_statmet_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_statmet
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

mod_statmet_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        div(id = ns("hideUI"),
            "The network has not been constructed yet"),
        div(id = ns("showUI"),
            fluidRow(
                column(8,
                       tabBox(
                           tabPanel(title = "Metrics table",
                                    DT::DTOutput(ns("metrics_table"))),
                           tabPanel(title = "Degrees histogram",
                                    plotOutput(ns("hist_degrees"))),
                           width = NULL)                       
                       ),
                column(4,
                       uiOutput(ns("stats_net")))
            ))
    )
}
    
# Module Server
    
#' @rdname mod_statmet
#' @export
#' @keywords internal
    
mod_statmet_server <- function(input, output, session, net) {
    ns <- session$ns

    observe({
        shinyjs::toggle("hideUI", condition = is.null(net()))
        shinyjs::toggle("showUI", condition = !is.null(net()))
    })
    
    stats_net = reactive({
        stats = list()
        stats$admissions = paste0("<b>Number of admissions:</b> ",
                            "TODO")
        stats$subjects = paste0("<b>Number of subjects:</b> ",
                          "TODO")
        stats$facilities = paste0("<b>Total number of facilities:</b> ",
                                  "TODO")
        stats$movements =  paste0("<b>Total number of movements:</b> ",
                                  net()$n_movements)
        stats$LOS = paste0("<b>Mean length of stay:</b> ",
                          "TODO",
                          " days")
        stats$TBA = paste0("<b>Mean time between admissions:</b> ",
                          "TODO",
                          " days")
        return(stats)
    })
        
    output$stats_net = renderUI({
        box(HTML(paste(stats_net(), collapse = "<br/>")),
            title = HTML("<b>Network statistics</b>"),
            icon = icon("clipboard-list"),
            width = NULL,
            collapsible = T,
            status = "info",
            solidHeader = T)
    })

    output$metrics_table = DT::renderDT(net()$metricsTable,
                                        filter = "bottom",
                                        options = list(scrollY = "400px",
                                                       scrollX = "300px"))

    output$hist_degrees = renderPlot(net()$plot("degree"))

    
}
    
## To be copied in the UI
# mod_statmet_ui("statmet_ui_1")
    
## To be copied in the server
# callModule(mod_statmet_server, "statmet_ui_1")
 
