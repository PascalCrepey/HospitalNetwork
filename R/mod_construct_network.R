# Module UI
  
#' @title   mod_construct_network_ui and mod_construct_network_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_construct_network
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @import shinyWidgets

mod_construct_network_ui <- function(id){
  ns <- NS(id)
  sbPan = sidebarPanel(
   h3("Parameters", align = "center"),
    div(id = ns("nobase"),
         "You must start by uploading and checking a database"),
     div(id = ns("parameters"),
         uiOutput(ns("window_threshold")),
        conditionalPanel(paste0("input['", ns("window"), "'] > 0"),
                         shinyWidgets::radioGroupButtons(ns("countoption"),
                                           label = "How to count connections",
                                           choices = c("Successive stays" = "successive",
                                                       "All stays" = "all"),
                                           selected = "successive",
                                           size = "normal")
        ),
        shinyWidgets::radioGroupButtons(ns("condition"),
                          label = "Conditioning on:",
                          choices = c("Dates" = "dates",
                                      "Flags" = "flags",
                                      "Both" = "both"),
                          selected = "dates",
                          size = "normal"),
        conditionalPanel(paste0("input['", ns("condition"), "'] == 'flags' | input['", ns("condition"), "'] == 'both'"),
                         uiOutput(ns("flagsUI"))
        ),
        fluidRow(
          column(5, HTML("<b>Remove loops</b></br>")),
          column(7, shinyWidgets::materialSwitch(ns("loops"), status = "primary", value = TRUE))
        ),
        fluidRow(
          column(5, HTML("<b>Transfer cutoff</b>")),
          column(7, shinyWidgets::materialSwitch(ns("nmoves"), status = "primary"))
        ),
        conditionalPanel(paste0("input['", ns("nmoves"), "'] == true"),
                         fluidRow(
                           column(5, textInput(ns("cutoff"),
                                               label = NULL,
                                               value = "NULL",
                                               width = "100%")),
                           column(7, htmlOutput(ns("cutoffcheck"),
                                                inline = TRUE))
                        )),
        actionButton(ns("construct"),
                     "Construct network",
                     icon = icon("wrench"),
                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ))
  
  
  sidebarLayout(
      mainPanel = mainPanel(
          includeMarkdown("inst/construct_network.md")
      ),
      sidebarPanel = sbPan,
      position = "right")

}
    
# Module Server
    
#' @rdname mod_construct_network
#' @export
#' @keywords internal
    
mod_construct_network_server <- function(input, output, session, base)
{
    ns <- session$ns

    observe({
        shinyjs::toggle("parameters", condition = !is.null(base()))
        shinyjs::toggle("nobase", condition = is.null(base()))
    })
    ##--- Render UI ----------------------------------------------------
    ## Window threshold
    output$window_threshold = renderUI({
        sliderInput(ns("window"),
                    label = "Window threshold",
                    min = 0,
                    max = as.integer(difftime(max(base()$Ddate),
                                              min(base()$Adate),
                                              units = "days")),
                    value = 14,
                    step = 1,
                    post = " days")
    })
    ## Flags selection
    output$flagsUI = renderUI({
        req(base())
        cols = colnames(base())
        list(
            HTML("<b>Flags for origins</b>"),
            fluidRow(
                column(6, "Variable"),
                column(6, "Value")),
            fluidRow(
                column(6, selectInput(ns("flagVarOrig"),
                                      label = NULL,
                                      choices = cols)),
                column(6, selectizeInput(ns("flagValueOrig"),
                                         label = NULL,
                                         choices = NULL,
                                         multiple = TRUE,
                                         options = list(create = TRUE)))),
            HTML("<b>Flags for targets</b>"),
            fluidRow(
                column(6, "Variable"),
                column(6, "Value")),
            fluidRow(
                column(6, selectInput(ns("flagVarTarg"),
                                      label = NULL,
                                      choices = cols)),
                column(6, selectizeInput(ns("flagValueTarg"),
                                         label = NULL,
                                         choices = NULL,
                                         multiple = TRUE,
                                         options = list(create = TRUE))))
        )
    })
    
    ##--- Check input for transfer cutoff -----------------------------------------------
    output$cutoffcheck = renderUI({
        req(base())
        if (input$cutoff != "") {
            check = as.numeric(input$cutoff)
            if (is.na(check) | check < 0 | check %% 1 != 0) {
                HTML('<font color="red"><b>Invalid:</b> must be a positive integer</font>')
            }
        } else {
            HTML("Please provide a positive integer")
        }
    })
    
    ##--- Construct network ---------------------------------------------------------------
    ## Set arguments
    args = eventReactive(input$construct, {
        list(nmoves_threshold = if (input$nmoves) as.numeric(input$cutoff) else NULL,
             flag_vars = if (input$condition == "dates") {
                             NULL
                         } else {
                             list("origin" = input$flagVarOrig,
                                  "target" = input$flagVarTarg)
                         },
             flag_values = if (input$condition == "dates") {
                               NULL
                           } else {
                               list("origin" = input$flagValueOrig,
                                    "target" = input$flagValueTarg)
                           }
             )
    })
    ## Construct
    savedMsg = c()
    savedWar = c()
    net = reactiveVal()
    out = eventReactive(input$construct, {
        withCallingHandlers(
            try(HospitalNetwork::hospinet_from_subject_database(
                                     base = base(),
                                     window_threshold = input$window,
                                     count_option = input$countoption,
                                     condition = input$condition,
                                     noloops = input$loops,
                                     nmoves_threshold = args()$nmoves_threshold,
                                     flag_vars = args()$flag_vars,
                                     flag_values = args()$flag_values, 
                                     shinySession = session)
                ),
            message = function(e) {
                savedMsg <<- c(savedMsg, conditionMessage(e))
            },
            warning = function(w) {
                savedWar <<- c(savedWar, warningCondition(w))
            }
        )
        
    })
    
    ##--- Pop-up message of success or failure ------------------------------------------
    observeEvent(input$construct, {
        savedMsg <<- c()
        savedWar <<- c()
        withProgress(message = "Building network...",
         net(out())
        )
        if ("try-error" %in% class(net())) {
            shinyalert::shinyalert(title = "Error",
                                   text = paste0("<div align=left>",
                                                 paste(paste(savedMsg, collapse = "</br>"),
                                                       "</br><b>", savedWar[[1]], "</br>",
                                                       "</br><b>", net()[[1]], "</b>"),
                                                 "</div><br/>"),
                                   type = "error",
                                   html = TRUE)
            net(NULL)
        }
        else {
            shinyalert::shinyalert(title = "Constructed successfully",
                                   text = HTML(paste0("<div align=left>",
                                                      "<b>With the following parameters:</b></br></br>",
                                                      "<ul>",
                                                      paste("<li>Window threshold:", input$window, "</li>"),
                                                      paste("<li>Count option:", input$countoption, "</li>"),
                                                      paste("<li>Condition:", input$condition, "</li>"),
                                                      paste("<li>Remove loops:", input$loops, "</li>"),
                                                      paste("<li>Transfer cutoff:", args()$nmoves_threshold, "</li>"),
                                                      paste("<li>Flag variables:", args()$flag_vars[[1]], args()$flag_vars[[2]], "</li>"),
                                                      paste("<li>Flag values:", args()$flag_values[[1]], args()$flag_values[[2]], "</li>"),
                                                      "</ul>",
                                                      "</div><br/>",
                                                      "<p style='color:salmon'><b>",
                                                      paste(savedWar, collapse = "</br>"),
                                                      "</b></p>")),
                                   type = "success",
                                   html = TRUE)
        }
    })
    return(reactive({ net() }))
}
    
## To be copied in the UI
# mod_construct_network_ui("construct_network_ui_1")
    
## To be copied in the server
# callModule(mod_construct_network_server, "construct_network_ui_1")
 
