# Module UI
  
#' @title   mod_loadNcheck_ui and mod_loadNcheck_server
#' @description  Module to load the database, perform checks, and print messages, warnings, and errors. Server returns the cleaned database.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_loadNcheck
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList
#' @import shinyWidgets

mod_loadNcheck_ui <- function(id) {
  ns <- NS(id)
  sbPan = sidebarPanel(
    tabsetPanel(
      tabPanel(title = "Upload and check dataset",
               fileInput(ns("dataset"),
                         "File",
                         buttonLabel = HTML(paste(icon("upload",  ), "Browse"))),
               fluidRow(
                 column(5, HTML("<b>File format</b>")),
                 column(7, 
                        shinyWidgets::radioGroupButtons(ns("filetype"),
                                                        choices = c("CSV", "Rdata", "RDS"),
                                                        status = "default",
                                                        size = "sm",
                                                        selected = NA)
                 )
               ),
               uiOutput(ns("checkUI"))
      ),
      tabPanel(title = "Fake dataset", 
               sliderInput(ns("fd_n_subjects"), "Number of subjects", 
                           min = 100, max = 10000, value = 1000),
               sliderInput(ns("fd_n_facilities"), "Number of facilities",
                           min = 2, max = 1000, value = 10),
               uiOutput(ns("fd_n_clustersUI")),
               div(style = "display: inline-block;vertical-align:top;",
                   actionButton(ns("buildFD"),
                                "Build base",
                                icon = icon("data-base"),
                                style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
               )
      )
    ),
    shinyjs::hidden(uiOutput(ns("downloadNgoto")))
  )

  sidebarLayout(
      mainPanel = mainPanel(
        uiOutput(ns("intro_md"))
      ),          
      sidebarPanel = sbPan,
      position = "right")

}
    
# Module Server
    
#' @rdname mod_loadNcheck
#' @export
#' @keywords internal
    
mod_loadNcheck_server <- function(input, output, session, parent, mainData){
    ns <- session$ns

    ## Reactive value to store the checked database
    base = reactiveVal()
    
    output$intro_md <- renderUI({
      includeMarkdown("inst/intro.md")
    })

    #--- Load file ----------------------------------------------------    
    dataset = reactive({
        req(input$dataset)
    })
    output$fileUploaded <- reactive({
        return(!is.null(dataset()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    #where we put the main datatable
    datatable = reactiveVal(NULL)
    #--- Read file depending on type ----------------------------------
    observeEvent(input$filetype,{
      req(input$filetype)
      #reset the base
      base(NULL)
      user_file = dataset()$datapath
      if (input$filetype == "CSV") {
        datatable(fread(user_file))
      }
      else if (input$filetype == "Rdata") {
          e = new.env()
          dtb = load(user_file, envir = e)
          datatable(e[[dtb]])
      }
      else {
        datatable(readRDS(user_file))
      }
    })
    
    # Generate the fake data ----
    observeEvent(input$buildFD, {
      withProgress(message = "Building fake data...",{
        if (input$fd_n_clusters == 1){
          print("build fake data with single cluster")
          db = create_fake_subjectDB(n_subjects = input$fd_n_subjects,
                                     n_facilities = input$fd_n_facilities,
                                     with_errors = FALSE)
          incProgress(amount = 0.5, detail = "data generated")
        }else{
          print("build fake data with multiple clusters")
          db = create_fake_subjectDB_clustered(n_subjects = input$fd_n_subjects, 
                                               n_facilities = input$fd_n_facilities,
                                               n_clusters = input$fd_n_clusters)
          incProgress(amount = 0.5, detail = "clustered data generated")
        }
        #output the base
        base(HospitalNetwork::checkBase(base = db))
        
        incProgress(amount = 0.5, detail = "data checked")
      })
      #update message board
      mainData$notifications = makeNotification(mainData$notifications, 
                                                mtype = "database", 
                                                micon = "battery-full", 
                                                mtext = "fake database loaded.")
      mainData$notifications = makeNotification(mainData$notifications, 
                                                mtype = "datafile", 
                                                micon = "file", 
                                                mtext = paste0("dataset: ",
                                                               input$fd_n_subjects,"/",
                                                               input$fd_n_facilities,"/",
                                                               input$fd_n_clusters))

      shinyalert::shinyalert(title = "Fake database built successfully",
                             text = paste0("The database containing ",
                                           input$fd_n_subjects, " subjects, ",
                                           input$fd_n_facilities, " facilities, and ",
                                           input$fd_n_clusters, " clusters has been built."),
                             type = "success")
      
      shinyjs::show(id = "downloadNgoto")

    })

    previously_checked = eventReactive(input$filetype, {
        inherits(datatable(), "hospinet.base")
    })
    
    ##--- Conditional UI to check database -----------------------------------
    output$checkUI = renderUI({
        req(input$filetype)
        req(input$dataset)
        validate(need(class(try(datatable())) != "try-error",
                      message = "Error reading file. Please verify file format"))
        validate(need("data.frame" %in% class(datatable()),
                      message = "Error: data provided is not a data.frame or data.table"))
        validate(need(ncol(datatable()) >= 4,
                      message = "Error: dataset must have at least 4 columns"))
        cols = colnames(datatable())
        conditionalUI = if (input$filetype == "CSV" |
                            (input$filetype == "Rdata" & !previously_checked()) |
                            (input$filetype == "RDS" & !previously_checked())) {
                            list(
                                fluidRow(
                                    column(5, HTML("<b>Subject identifier:</b>")),
                                    column(7, selectInput(ns("sID"),
                                                          label = NULL,
                                                          choices = cols,
                                                          selected = cols[1]))
                                ),
                                fluidRow(
                                    column(5, HTML("<b>Facility identifier:</b>")),
                                    column(7, selectInput(ns("fID"),
                                                          label = NULL,
                                                          choices = cols,
                                                          selected = cols[2]))
                                ),
                                fluidRow(
                                    column(5, HTML("<b>Admission date</b>")),
                                    column(7, selectInput(ns("adate"),
                                                          label = NULL,
                                                          choices = cols,
                                                          selected = cols[3]))
                                ),
                                fluidRow(
                                    column(5, HTML("<b>Discharge date</b>")),
                                    column(7, selectInput(ns("ddate"),
                                                          label = NULL,
                                                          choices = cols,
                                                          selected = cols[4]))
                                ),
                                fluidRow(
                                    column(5, HTML("<b>Date format</b>")),
                                    column(7, selectInput(ns("dateformat"),
                                                          label = NULL,
                                                          choices = c("Year-Month-Day" = "ymd",
                                                                      "Year-Day-Month" = "ydm",
                                                                      "Month-Day-Year" = "mdy",
                                                                      "Day-Month-Year" = "dmy",
                                                                      "Day-Year-Month" = "dym",
                                                                      "Month-Year-Day" = "myd"),
                                                          selected = "Year-Month-Day"))
                                ),
                                fluidRow(
                                    column(5, HTML("<b>Time format</b>")),
                                    column(7, selectInput(ns("timeformat"),
                                                          label = NULL,
                                                          choices = c("None" = "",
                                                                      "Hours-Minutes-Seconds" = "HMS",                                                                    "Hours-Minutes-Seconds" = "HMS",
                                                                      "Seconds-Minutes-Hours" = "SMH",
                                                                      "Hours-Minutes" = "HM"),
                                                          selected = "None"))
                                ),
                                shinyWidgets::awesomeRadio(ns("deletemissing"),
                                             "Remove missing values?",
                                             choices = c("No" = "No",
                                                         "Remove record" = "record",
                                                         "Remove patient" = "patient"),
                                             inline = T),
                                shinyWidgets::awesomeRadio(ns("deleteerrors"),
                                             "Remove erroneous records?",
                                             choices = c("No" = "No",
                                                         "Remove record" = "record",
                                                         "Remove patient" = "patient"),
                                             inline = T)
                            )
                        } else {
                            NULL
                        }
        startUI = list(
            div(style="display: inline-block;vertical-align:top;",
                actionButton(ns("start"),
                             "Check base",
                             icon = icon("search"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                )            
        )
        return(conditionalPanel(paste0("output['", ns("fileUploaded"), "']"),
                                c(conditionalUI, startUI)))
    })
    
    output$downloadNgoto <- renderUI({
      dgUI = div(style="display: inline-block;vertical-align:top;padding-top:10px",
                           downloadButton(ns("downloadCheckedBase"),
                                          label = "Download checked base"),
                           actionButton(ns("goToConstruct"), "go to network", icon = icon("forward"))
      )
      return(dgUI)
    })

    #--- Set arguments ------------------------------------------------
    args = reactive({
        list(deleteMissing = if(input$deletemissing == "No") NULL else input$deletemissing,
             deleteErrors = if(input$deleteerrors == "No") NULL else input$deleteerrors,
             datetimeformat = paste0(input$dateformat, input$timeformat))
    })
    
    #--- Check dataset, with error/message/warning catch ---------------
    savedMsg = c()
    savedWar = c()
    checked = eventReactive(input$start, {
        base(NULL)
        if (!previously_checked()) {
        out = withCallingHandlers(
            try(HospitalNetwork::checkBase(base = datatable(),
                                           convertDates = T,
                                           dateFormat = args()$datetimeformat,
                                           deleteMissing = args()$deleteMissing,
                                           deleteErrors = args()$deleteErrors,
                                           subjectID = input$sID,
                                           facilityID = input$fID,
                                           admDate = input$adate,
                                           disDate = input$ddate,
                                           returnReport = T,
                                           verbose = T)),
            message = function(e) {
                savedMsg <<- c(savedMsg, conditionMessage(e))
            },
            warning = function(w) {
                savedWar <<- c(savedWar, warningCondition(w))
            }
        )
        return(out)
        } else {
            return(NULL)
        }
    })

    #--- Success or failure ---------------------------
    observeEvent(input$start, {
        savedMsg <<- c()
        savedWar <<- c()
        out = checked()
        if (previously_checked()) {
            base(datatable())
          #browser()
            #update message board
            mainData$notifications = makeNotification(mainData$notifications, 
                                                      mtype = "database", 
                                                      micon = "battery-full", 
                                                      mtext = "database checked and loaded.")
            mainData$notifications = makeNotification(mainData$notifications, 
                                                      mtype = "datafile", 
                                                      micon = "file", 
                                                      mtext = paste0("dataset: ",dataset()$name))
            #browser()
            shinyalert::shinyalert(title = "Database previously checked",
                                   text = "The database was not checked again",
                                   type = "success")
            shinyjs::show(id = "downloadNgoto")
        } else if ("try-error" %in% class(out)) {
            shinyalert::shinyalert(title = "Error",
                                   text = paste0("<div align=left>",
                                                 paste(paste(savedMsg, collapse = "</br>"),
                                                       "</br><b>", savedWar[[1]], "</br>",
                                                       "</br><b>", out[[1]], "</b>"),
                                                 "</div><br/>"),
                                   type = "error",
                                   html = T)
        } else {
            base(checked())
            totaldeleted = sum(unlist(attr(base(), "report")[c("removedErrors",
                                                               "removedMissing",
                                                               "removedDuplicates")]
                                      )
                               )
            #update message board
            mainData$notifications = makeNotification(mainData$notifications, mtype = "database", micon = "battery-full",
                             mtext = "database checked and loaded !")
            mainData$notifications = makeNotification(mainData$notifications, 
                                                      mtype = "datafile", 
                                                      micon = "file", 
                                                      mtext = paste0("dataset: ",dataset()$name))
            
            shinyalert::shinyalert(title = "Checked",
                                   text = HTML(paste0("<div align=left>",
                                                      paste(savedMsg, collapse = "<br/>"),
                                                      "</div><br/>",
                                                      "<p style='color:salmon'><b>",savedWar[[1]], "</b></p></br>",
                                                      paste0("<b>Total number of deleted records: ", totaldeleted, "</b>"))),
                                   type = "success",
                                   html = T)
            shinyjs::show(id = "downloadNgoto")
        }
    })

    ## ##--- Propose to download cleaned data ---------------------------

    
    observeEvent(input$goToConstruct, {
      shinydashboard::updateTabItems(session = parent, inputId = "mainSidebar", selected = "construct")
    })
    output$downloadCheckedBase <- downloadHandler(
        filename = function() {
          "checked_database.RDS"
        },
        content = function(file) {
            saveRDS(base(), file)
        }
    )

    # Fake data UI ----
    output$fd_n_clustersUI <- renderUI({
      sliderInput(ns("fd_n_clusters"), "number of clusters", 
                  min = 1, 
                  max = input$fd_n_facilities, step = 1,
                  value = 1)
    })
    


    ##--- RETURN ------------------------------------------------------
    return(reactive({base()}))


}
    
## To be copied in the UI
# mod_loadNcheck_ui("loadNcheck_ui_1")
    
## To be copied in the server
# callModule(mod_loadNcheck_server, "loadNcheck_ui_1")
 
