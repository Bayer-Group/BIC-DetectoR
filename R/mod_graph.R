#' graph UI Function
#' @description A shiny Module.
#' @inheritParams mod_upload_ui
#' @noRd
mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    mod_calculate_ui("calculate_double_dot", calculate_mode = "double_dot"),
    shiny::conditionalPanel(
      condition = "!output.double_dot_plot",
      ns = ns,
      shiny::span(
        shiny::icon("circle-info"),
        "Click the Calculate! button to display the plot."
      )
    ),
    col_12(
      # Plot headers ----
      shiny::fluidRow(
        col_7(
          shiny::uiOutput(ns("double_dot_plot_header")),
          class = "plot_header double_dot_plot_header"
        ),
        col_5(
          shiny::textOutput(ns("effect_plot_header")),
          class = "plot_header effect_plot_header"
        )
      ),
      # Plot outputs ----
      shiny::fluidRow(
        col_7(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("double_dot_plot")),
            type = 4
          )
        ),
        col_5(
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("effect_plot")),
            type = 4
          )
        )
      ),
      shiny::conditionalPanel(
        condition = "output.double_dot_plot",
        ns = ns,
        shiny::fluidRow(
          col_7(
            shiny::span(
              shiny::icon("circle-info"),
              "**: significant by adjusted p-value; *: significant only by
              unadjusted p-value.",
              "Colors: "
            ),
            shiny::span("Favours comparator", class = "comparator-col"),
            shiny::span(", "),
            shiny::span("Favours verum", class = "verum-col"),
            shiny::span(", Non-significant. "),
            shiny::span("Subjects are counted once per category.")
          ),
          col_5(
            shiny::span(
              shiny::icon("circle-info"),
              "CI: Confidence Interval; RD: Risk Difference; RR: Risk Ratio."
            )
          )
        ),
        shiny::br(),
        shiny::wellPanel(
          shiny::span(shiny::icon("filter"), "Filters applied:"),
          shiny::textOutput(ns("filter_list_adae")),
          shiny::textOutput(ns("filter_list_adsl"))
        )
      )
    )
  )
}

#' graph Server Function
#'
#' Needs the following objects on r: verum_name, comparator_name,
#' double_dot_data, effect_measure, alpha, adjustment_method, frequency_measure,
#'  stratified_by, label_collapsed
#'
#' @inheritParams mod_upload_server
#' @noRd
mod_graph_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Plot headers ----
    ## Double dot plot header ----
    output$double_dot_plot_header <- shiny::renderUI({
      shiny::req(r$double_dot_data, r$verum_name, r$comparator_name)
      shiny::HTML(
        paste0(
          "Double Dot Plot &nbsp; &nbsp; &nbsp; &nbsp;",
          "<span class='verum-col'>&#9679;", # Circle
          r$verum_name,
          "</span>",
          "<span class='comparator-col'>&#9650;", # Triangle
          r$comparator_name,
          "</span>"
        )
      )
    }) |>
      shiny::bindEvent(r$go_double_dot)
    ## Effect plot header ----
    output$effect_plot_header <- shiny::renderText({
      shiny::req(r$double_dot_data, r$effect_measure, r$alpha)
      confidence_level <- 100 * (1 - r$alpha)
      if (r$effect_measure == "RR") {
        text <- "Relative Risk"
      } else if (r$effect_measure == "RD") {
        text <- "Risk Difference"
      }
      paste0(text, " with ", confidence_level, "% CI")
    }) |>
      shiny::bindEvent(r$go_double_dot)
    # Render plots ----
    height_plot <- shiny::reactive({
      shiny::req(r$double_dot_data)
      rows <- nrow(r$double_dot_data) / 2
      height <- 150 + rows * 20
      height
    })
    ## Double dot plot ----
    output$double_dot_plot <- plotly::renderPlotly({
      shiny::req(r$frequency_measure, height_plot())
      validate_need(
        r$double_dot_data,
        "Nothing to display. Please click on Calculate!"
      )
      validate_need(
        nrow(r$double_dot_data) > 0,
        "Filtered data has zero rows! Please check the active filters."
      )
      make_double_dot_plotly(
        data = r$double_dot_data,
        label_collapsed = r$label_collapsed,
        frequency_measure = r$frequency_measure,
        height = height_plot()
      )
    }) |>
      shiny::bindEvent(r$go_double_dot, r$label_collapsed)
    ## Effect measure plot ----
    output$effect_plot <- plotly::renderPlotly({
      shiny::req(r$effect_measure, height_plot())
      validate_need(
        r$double_dot_data,
        "Nothing to display. Please click on Calculate!"
      )
      validate_need(
        nrow(r$double_dot_data) > 0,
        "Filtered data has zero rows! Please check the active filters."
      )
      make_effect_plotly(
        data = r$double_dot_data,
        display = r$effect_measure,
        adjustment = r$adjustment_method,
        height = height_plot()
      )
    }) |>
      shiny::bindEvent(r$go_double_dot)
    # Filters applied
    output$filter_list_adae <- shiny::renderText(r$filter_list_adae) |>
      shiny::bindEvent(r$go_double_dot)
    output$filter_list_adsl <- shiny::renderText(r$filter_list_adsl) |>
      shiny::bindEvent(r$go_double_dot)
  })
}
