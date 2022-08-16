#' start webapp
#'
#' @export
#' @rdname startApplication
start_app <- function() {
   appDir <- system.file("/app", package = "DetectoR")
   shiny::runApp(appDir, launch.browser = TRUE)
}