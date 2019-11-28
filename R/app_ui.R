#' @import shiny
#' @import shinydashboard
#' 
app_ui <- function() {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    dashboardPage(
        dashboardHeader(title = "HospitalNetwork",
                        titleWidth = "200px"),
        dashboardSidebar(
            sidebarMenu(id = "mainSidebar",
                menuItem("Upload and check",
                         tabName = "upload",
                         icon = icon("upload")),
                menuItem("Construct network",
                         tabName = "construct",
                         icon = icon("wrench")),
                menuItem("Database",
                         tabName = "database",
                         icon = icon("database")),
                menuItem("Network",
                         tabName = "network",
                         icon = icon("connectdevelop")),
                menuItem("Statistics and metrics",
                         tabName = "stat",
                         icon = icon("clipboard-list"))
                ), width = "200px"
        ),
        dashboardBody(
            shinyjs::useShinyjs(),
            shinyalert::useShinyalert(),
            tabItems(
                tabItem(tabName = "upload",
                        mod_loadNcheck_ui("loadNcheck_ui_1")
                        ),
                tabItem(tabName = "construct",
                        mod_construct_network_ui("construct_network_ui_1")
                        ),
                tabItem(tabName = "database",
                        mod_database_ui("database_ui_1")
                        ),
                tabItem(tabName = "network",
                        mod_network_ui("network_ui_1")
                        ),
                tabItem(tabName = "stat",
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



