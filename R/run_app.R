#' Run the Shiny Application
#'
#' @param ... parameters passed on to golem options.
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server), 
    golem_opts = list(...)
  )
}

#' Launch app
#' @export
#' 
shiny_app = function() {
    options( "golem.app.prod" = TRUE)
    run_app() # add parameters here (if any)
}

makeNotification = function(notifications, mtype, micon, mtext){
  tmpNotif = copy(notifications)
  tmpNotif[type == mtype, ':='(icon = micon, text = mtext)]
  #update the pointer to trigger interface update
  tmpNotif
}