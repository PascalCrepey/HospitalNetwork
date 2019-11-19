#' @import shiny
#' 
options(shiny.maxRequestSize=150*1024^2)

app_server <- function(input, output,session) {
    # List the first level callModules here
    base = callModule(mod_loadNcheck_server, "loadNcheck_ui_1")
    callModule(mod_database_server, "database_ui_1", base)
    net = callModule(mod_construct_network_server, "construct_network_ui_1", base)
    callModule(mod_network_server, "network_ui_1", net)
    callModule(mod_statmet_server, "statmet_ui_1", net)

    
}
