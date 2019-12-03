#' @import shiny
#' @import shinydashboard
#' 
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "HospitalNetwork",
                        titleWidth = "200px", 
                        shinydashboard::dropdownMenuOutput("messageMenu")),
      shinydashboard::dashboardSidebar(
        shinydashboard::sidebarMenu(id = "mainSidebar",
                                    shinydashboard::menuItem("Upload and check",
                         tabName = "upload",
                         icon = icon("upload")),
                         shinydashboard::menuItem("Construct network",
                         tabName = "construct",
                         icon = icon("wrench")),
                         shinydashboard::menuItem("Database",
                         tabName = "database",
                         icon = icon("database")),
                         shinydashboard::menuItem("Network",
                         tabName = "network",
                         icon = icon("connectdevelop")),
                         shinydashboard::menuItem("Statistics and metrics",
                         tabName = "stat",
                         icon = icon("clipboard-list"))
                ), width = "200px"
        ),
      shinydashboard::dashboardBody(
            shinyjs::useShinyjs(),
            shinyalert::useShinyalert(),
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "upload",
                        mod_loadNcheck_ui("loadNcheck_ui_1")
                        ),
              shinydashboard::tabItem(tabName = "construct",
                        mod_construct_network_ui("construct_network_ui_1")
                        ),
              shinydashboard::tabItem(tabName = "database",
                        mod_database_ui("database_ui_1")
                        ),
              shinydashboard::tabItem(tabName = "network",
                        mod_network_ui("network_ui_1")
                        ),
              shinydashboard::tabItem(tabName = "stat",
                        mod_statmet_ui("statmet_ui_1")
                        )
                 )
        )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'HospitalNetwork')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}



