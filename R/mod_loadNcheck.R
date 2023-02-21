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
                         "File (transfers or already checked database)",
                         buttonLabel = "Browse"),
               checkboxInput(ns("gps_load"), HTML("<b>Add dataset with GPS coordinates and bed capacities</b>")),
               uiOutput(ns("gps_loadUI")),
               actionButton(ns("loadfiles"), "Load files", icon = icon("upload")),
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
#' @importFrom openxlsx read.xlsx
#' @importFrom readxl read_xls
    
mod_loadNcheck_server <- function(input, output, session, parent, mainData){
    ns <- session$ns

    ## Reactive value to store the checked database
    base = reactiveVal()
    # gps_dataset = reactiveVal()
    
    output$intro_md <- renderUI({
      includeMarkdown("inst/intro.md")
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
        #output the two bases
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
    
    #--- Real data from the user ----------------------------------------------------    
    dataset = reactive({
        req(input$dataset)
    })
    output$fileUploaded <- reactive({
        return(!is.null(dataset()))
    })
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

    gps_dataset = reactive({
      req(input$gps_dataset)
    })
    output$fileUploaded_gps <- reactive({
      return(!is.null(gps_dataset()))
    })
    outputOptions(output, 'fileUploaded_gps', suspendWhenHidden=FALSE)

    
    #--- Loading the datasets provided by the user ---------------------------------
    datatable = reactiveVal(NULL)
    datatable_gps = reactiveVal(NULL)
    # Read file depending on type #
    observeEvent(input$loadfiles,{
      req(input$loadfiles)
      #reset the base
      base(NULL)
      # Hospital stays
      user_file = dataset()$datapath
      extension_file = unlist(strsplit(user_file, split = "\\."))[2]
      if(!extension_file %in% c("csv", "xlsx", "xls", "Rdata", "RData", "rds", "RDS")){
        shinyalert::shinyalert(title = "Error",
                               text = paste0("<div align=left>Error reading file. <br>Format must be one of csv, xlsx, xls, Rdata, RData or rds</div>"),
                               type = "error",
                               html = T)
      }
      else if (extension_file == "csv") {
        datatable(fread(user_file))
      }
      else if (extension_file == "xlsx") {
        datatable(as.data.table(read.xlsx(user_file)))
      }
      else if (extension_file == "xls") {
        datatable(as.data.table(read_xls(user_file)))
      }
      else if (extension_file %in% c("Rdata", "RData")) {
          e = new.env()
          dtb = load(user_file, envir = e)
          datatable(e[[dtb]])
      }
      else {
        datatable(readRDS(user_file))
      }
      
      if(input$gps_load){
        # GPS and beds data
        gps_user_file = gps_dataset()$datapath
        gps_extension_file = unlist(strsplit(gps_user_file, split = "\\."))[2]
        if(!gps_extension_file %in% c("csv", "xlsx", "xls", "Rdata", "RData", "rds", "RDS")){
          shinyalert::shinyalert(title = "Error",
                                 text = paste0("<div align=left>Error reading GPS file. <br>Format must be one of csv, xlsx, xls, Rdata, RData or rds</div>"),
                                 type = "error",
                                 html = T)
        }
        else if (gps_extension_file == "csv") {
          datatable_gps(fread(gps_user_file))
        }
        else if (gps_extension_file == "xlsx") {
          datatable_gps(as.data.table(read.xlsx(gps_user_file)))
        }
        else if (gps_extension_file == "xls") {
          datatable_gps(as.data.table(read_xls(gps_user_file)))
        }
        else if (gps_extension_file %in% c("Rdata", "RData")) {
          e = new.env()
          dtb = load(gps_user_file, envir = e)
          datatable_gps(e[[dtb]])
        }
        else {
          datatable_gps(readRDS(gps_user_file))
        }
      }
    })
    
    previously_checked = eventReactive(input$loadfiles, {
        inherits(datatable(), "hospinet.base")
    })

    
    ##--- Conditional UI to check database -----------------------------------
    output$checkUI = renderUI({
        req(input$loadfiles)
        req(input$dataset)
        # Transfer dataset #
        validate(need(class(try(datatable())) != "try-error",
                      message = "Error reading file. Please verify file format"))
        validate(need("data.frame" %in% class(datatable()),
                      message = "Error: data provided is not a data.frame or data.table"))
        validate(need(ncol(datatable()) >= 4,
                      message = "Error: dataset must have at least 4 columns"))
        cols = colnames(datatable())
        # GPS data #
        if(input$gps_load){
          validate(need(class(try(datatable_gps())) != "try-error",
                        message = "Error reading file. Please verify file format"))
          validate(need("data.frame" %in% class(datatable_gps()),
                        message = "Error: data provided is not a data.frame or data.table"))
          validate(need(ncol(datatable_gps()) >= 4,
                        message = "Error: dataset must have at least 4 columns"))
          cols_gps = colnames(datatable_gps())
        }
        
        conditionalUI = if (!previously_checked() & !input$gps_load) {
                            list(
                              HTML("<b>Columns check for file\n</b>"),
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
                                             inline = T))
                        } else if(!previously_checked()) {
                          list(
                            HTML("<b>\nColumns check for file\n</b>"),
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
                                                       inline = T),
                            HTML("<b>Columns check for GPS file\n</b>"),
                            fluidRow(
                              column(5, HTML("<b>Facility identifier:</b>")),
                              column(7, selectInput(ns("fID"),
                                                    label = NULL,
                                                    choices = cols_gps,
                                                    selected = cols_gps[1]))
                            ),
                            fluidRow(
                              column(5, HTML("<b>Latitude:</b>")),
                              column(7, selectInput(ns("lat"),
                                                    label = NULL,
                                                    choices = cols_gps,
                                                    selected = cols_gps[2]))
                            ),
                            fluidRow(
                              column(5, HTML("<b>Longitude</b>")),
                              column(7, selectInput(ns("long"),
                                                    label = NULL,
                                                    choices = cols_gps,
                                                    selected = cols_gps[3]))
                            ),
                            fluidRow(
                              column(5, HTML("<b>Number of beds</b>")),
                              column(7, selectInput(ns("beds"),
                                                    label = NULL,
                                                    choices = cols_gps,
                                                    selected = cols_gps[4]))
                            ))
                            
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
    
    ##--- Conditional UI to load GPS data -----------------------------------
    output$gps_loadUI = renderUI({
      req(input$gps_load)
      fileInput(ns("gps_dataset"),
                "File with GPS coord. and beds",
                buttonLabel = "Browse")
    })
    
    output$downloadNgoto <- renderUI({
      dgUI = div(style="display: inline-block;vertical-align:top;padding-top:10px",
                           downloadButton(ns("downloadCheckedBase"),
                                          label = "Download checked base"),
                           actionButton(ns("goToConstruct"), "Go to network", icon = icon("forward"))
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
            try(HospitalNetwork::checkBase(base = list(all_s_stays = datatable(),
                                                       gps_facilities = datatable_gps()),
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
            #update message board
            mainData$notifications = makeNotification(mainData$notifications, 
                                                      mtype = "database", 
                                                      micon = "battery-full", 
                                                      mtext = "database checked and loaded.")
            mainData$notifications = makeNotification(mainData$notifications, 
                                                      mtype = "datafile", 
                                                      micon = "file", 
                                                      mtext = paste0("dataset: ",dataset()$name))
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
 
