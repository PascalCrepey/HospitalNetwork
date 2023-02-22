# Module UI
  
#' @title   mod_database_ui and mod_database_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_database
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

mod_database_ui <- function(id){

    ns <- NS(id)

    tagList(
        div(id = ns("hideUI"),
            "You must start by uploading and checking a database"),
        div(id = ns("showUI"),
            tabsetPanel(
                tabPanel(
                    title = "Database",
                    DT::dataTableOutput(ns("base"))
                ),
                tabPanel(
                    title = "Summary statistics",
                    fluidRow(
                        column(7,
                               h2("Statistics by facilities"),
                               DT::DTOutput(ns("byfacil"))),
                        column(5,
                               uiOutput(ns("stats_base")))
                    )
                )
            ))
    )
}
    
# Module Server
    
#' @rdname mod_database
#' @export
#' @keywords internal
    
mod_database_server <- function(input, output, session, base)
{
    ns <- session$ns

    observe({
        shinyjs::toggle("hideUI", condition = is.null(base()))
        shinyjs::toggle("showUI", condition = !is.null(base()))
    })
    

    output$base = DT::renderDT(
                          expr = {
                              validate(need(base(),
                                            "You must start by uploading and checking a database"))
                              base()
                          },
                          filter = "bottom",
                          options = list(scrollY = "400px"))

    byfacil = reactive({
        tmp = HospitalNetwork::per_facility_summary(base()) 
        tmp[, LOS := as.difftime(tim = round(LOS, digits = 2), units = "days")]
        setnames(tmp,
                 old = c("node", "LOS", "admissions", "subjects"),
                 new = c("Facility ID", "Mean LOS", "Total number of admissions", "Distinct subjects admitted")
                 )
        return(tmp)
    })
    
    stats_base = reactive({
        out = HospitalNetwork::all_admissions_summary(base())
        vals = list()
        vals$admissions = paste0("<b>Number of admissions:</b> ",
                                 out$totalAdmissions)
        vals$subjects = paste0("<b>Number of subjects:</b> ",
                               out$numSubjects)
        vals$facilities = paste0("<b>Total number of facilities:</b> ",
                                 out$numFacilities)
        vals$LOS = paste0("<b>Mean length of stay:</b> ",
                          round(out$meanLOS, digits = 2),
                          " days")
        vals$TBA = paste0("<b>Mean time between admissions:</b> ",
                          round(out$meanTBA, digits = 2),
                          " days")
        return(vals)
    })
        
    output$byfacil = DT::renderDT(byfacil(),
                                  filter = "bottom",
                                  options = list(scrollY = "400px"))
    output$stats_base = renderUI({
        box(HTML(paste(stats_base(), collapse = "<br/>")),
            title = HTML("<b>Database statistics</b>"),
            icon = icon("clipboard-list"),
            width = NULL,
            collapsible = T,
            status = "info",
            solidHeader = T)
    })
    
}
    
## To be copied in the UI
# mod_database_ui("database_ui_1")
    
## To be copied in the server
# callModule(mod_database_server, "database_ui_1")
 
