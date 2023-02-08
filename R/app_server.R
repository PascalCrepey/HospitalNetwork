#' @import shiny
#'
options(shiny.maxRequestSize = 150*1024^2)

app_server <- function(input, output, session) {
    #general notifications
    mainData = reactiveValues(
        notifications = data.table(type = c("database", "datafile"),
                                   icon = c("battery-empty","file"),
                                   text = c("No database.","No file loaded")),
        database = NULL,
        networks = NULL,
        selectedNetwork = 0
    )
    # List the first level callModules here
    base = callModule(mod_loadNcheck_server, "loadNcheck_ui_1", parent = session, mainData = mainData)
    callModule(mod_database_server, "database_ui_1", base)
    net = callModule(mod_construct_network_server, "construct_network_ui_1", base)
    callModule(mod_network_server, "network_ui_1", net)
    callModule(mod_statmet_server, "statmet_ui_1", net)
    callModule(mod_map_server, "mod_map_ui_1", net)

    output$messageMenu <- renderMenu({
        # Code to generate each of the messageItems here, in a list. This assumes
        # that messageData is a data frame with two columns, 'from' and 'message'.
        msgs <- apply(mainData$notifications, 1, function(row) {
            notificationItem(icon = icon(row[["icon"]]), text = row[["text"]])
        })

        dropdownMenu(type = "notifications", .list = msgs)
    })
}
