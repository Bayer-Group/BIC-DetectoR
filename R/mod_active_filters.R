#' info UI Function
#'
#' @description A shiny Module.
#'
#' @inheritParams mod_upload_ui
#'
#' @noRd
#'
mod_active_filters_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::accordion(
    bslib::accordion_panel(
      title = "Active Filters:",
      icon = shiny::icon("filter"),
      shiny::textOutput(ns("filter_list_adae")),
      shiny::textOutput(ns("filter_list_adsl"))
    )
  )
}

#' info Server Functions
#' @inheritParams mod_upload_server
#' @param trigger Button to trigger the update of info (e.g., r$go_double_dot)
#' @noRd
mod_active_filters_server <- function(id, r, trigger) {
  shiny::moduleServer(id, function(input, output, session) {
    # Button trigger
    go <- shiny::reactive({
      trigger
    })
    # Filters applied
    output$filter_list_adae <- shiny::renderText({
      r$filter_list_adae
    }) |>
      shiny::bindEvent(go)
    output$filter_list_adsl <- shiny::renderText({
      r$filter_list_adsl
    }) |>
      shiny::bindEvent(go)
  })
}
