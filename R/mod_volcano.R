#' volcano UI Function
#'
#' @description A shiny Module for the volcano plot in DetectoR - user interface
#'
#' @inheritParams mod_upload_ui
#'
mod_volcano_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_calculate_ui("calculate_volcano", calculate_mode = "volcano"),
    shiny::conditionalPanel(
      condition = "!output.volcano_plot",
      ns = ns,
      shiny::span(
        shiny::icon("circle-info"),
        "Click the Calculate! button to display the plot."
      )
    ),
    ## Display volcano plot ----
    shinycssloaders::withSpinner(
      plotly::plotlyOutput(ns("volcano_plot"), height = 600),
      type = 4
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.volcano_plot",
      ns = ns,
      shiny::wellPanel(
        shiny::span(shiny::icon("filter"), "Filters applied:"),
        shiny::textOutput(ns("filter_list_adae")),
        shiny::textOutput(ns("filter_list_adsl"))
      )
    )
  )
}

#' Volcano plot server function
#'
#' @inheritParams mod_upload_server
#'
#' @description A shiny Module for the volcano plot in DetectoR - server part
#'
mod_volcano_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    output$volcano_plot <- plotly::renderPlotly({
      shiny::req(r$volcano_data, r$effect_measure, r$alpha)
      validate_need(
        nrow(r$volcano_data) > 0,
        "Filtered data has zero rows! Please check the active filters."
      )
      draw_volcano(
        data = r$volcano_data,
        effect_measure = r$effect_measure,
        verum_name = r$verum_name,
        comparator_name = r$comparator_name,
        alpha = r$alpha
      )
    }) |>
      shiny::bindEvent(r$go_volcano)
    # Filters applied
    output$filter_list_adae <- shiny::renderText(r$filter_list_adae) |>
      shiny::bindEvent(r$go_volcano)
    output$filter_list_adsl <- shiny::renderText(r$filter_list_adsl) |>
      shiny::bindEvent(r$go_volcano)
  })
}
