#' graph UI Function
#' @description A shiny Module.
#' @inheritParams mod_upload_ui
#' @noRd
mod_graph_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    # Plot headers
    bslib::card_header(
      bslib::layout_columns(
        # TODO: move to external css
        style = "width: 100%",
        col_widths = c(7, 5),
        shiny::div(
          shiny::uiOutput(ns("double_dot_plot_header"))
        ),
        shiny::div(
          shiny::textOutput(ns("effect_plot_header"))
        )
      )
    ),
    bslib::card_body(
      # Placeholder
      shiny::conditionalPanel(
        condition = "!output.double_dot_plot",
        ns = ns,
        shiny::span(
          shiny::icon("circle-info"),
          "Click the Calculate! button to display the plot."
        )
      ),
      bslib::layout_columns(
        col_widths = c(7, 5),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("double_dot_plot")),
          type = 4
        ),
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("effect_plot")),
          type = 4
        )
      )
    ),
    # Footer
    bslib::card_footer(
      shiny::conditionalPanel(
        condition = "output.double_dot_plot",
        ns = ns,
        bslib::layout_columns(
          # TODO: move to css
          style = "width: 100%",
          col_widths = c(7, 5),
          # Double dot plot footer
          shiny::div(
            shiny::icon("circle-info"),
            shiny::span("**: significant by adjusted p-value; "),
            shiny::span("*: significant only by unadjusted p-value. Colors: "),
            shiny::span("Favours comparator", class = "comparator-col"),
            shiny::span(", "),
            shiny::span("Favours verum", class = "verum-col"),
            shiny::span(", Non-significant. "),
            shiny::span("Subjects are counted once per category.")
          ),
          # Effect plot footer
          shiny::div(
            shiny::span(
              shiny::icon("circle-info"),
              "CI: Confidence Interval; RD: Risk Difference; RR: Risk Ratio."
            )
          )
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
  })
}
