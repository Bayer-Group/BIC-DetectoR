#' heatmap UI Function
#' @description A shiny Module.
#' @inheritParams mod_upload_ui
#' @noRd
mod_heatmap_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    full_screen = TRUE,
    bslib::card_header("Heatmap"),
    bslib::card_body(
      # Placeholder
      invisible_text(ns("heatmap_ready")),
      shiny::conditionalPanel(
        condition = "!output.heatmap_ready",
        ns = ns,
        shiny::span(
          shiny::icon("circle-info"),
          "Click the Calculate! button to display the plot."
        )
      ),
      ## Display heatmap ----
      shinycssloaders::withSpinner(
        plotly::plotlyOutput(ns("heatmap_plot")),
        type = 4
      )
    ),
    bslib::card_footer(
      shiny::conditionalPanel(
        condition = "output.heatmap_plot",
        ns = ns,
        plotly::plotlyOutput(ns("heatmap_legend"), height = 150),
        shiny::span(
          shiny::icon("circle-info"),
          "Click on any cell to zoom in, and on the treemap header to zoom out."
        )
      )
    )
  )
}

#' heatmap Server Function
#' #' Needs the following objects on r: verum_name, comparator_name,
#' heatmap_data, heatmap_color, alpha
#' @inheritParams mod_upload_server
#' @noRd
mod_heatmap_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Create treemap (heatmap) plot ----
    treemap <- shiny::reactive({
      shiny::req(
        r$go_heatmap,
        r$heatmap_color,
        r$verum_name,
        r$comparator_name,
        r$alpha,
        r$theme
      )
      validate_need(
        nrow(r$heatmap_data) > 0,
        "Filtered data has zero rows! Please check the active filters."
      )
      # Add details for heatmap
      heatmap_data <- r$heatmap_data |>
        add_heatmap_hover_text(
          heatmap_color = r$heatmap_color,
          verum_name = r$verum_name,
          comparator_name = r$comparator_name
        ) |>
        add_heatmap_colors(
          heatmap_mode = r$heatmap_color,
          alpha = r$alpha,
          theme = r$theme
        )
      heatmap_data
    }) |>
      shiny::bindEvent(r$go_heatmap, r$theme)
    treemap_legend <- shiny::reactive({
      shiny::req(
        treemap(),
        r$verum_name,
        r$comparator_name,
        r$alpha,
        r$theme
      )
      make_treemap_legend(
        data = treemap(),
        heatmap_color = r$heatmap_color,
        alpha = r$alpha,
        theme = r$theme,
        verum_name = r$verum_name,
        comparator_name = r$comparator_name
      )
    }) |>
      shiny::bindEvent(r$go_heatmap, r$theme)
    output$heatmap_plot <- plotly::renderPlotly({
      shiny::req(
        treemap(),
        r$heatmap_color,
        r$theme
      )
      draw_treemap(
        data = treemap(),
        heatmap_color = r$heatmap_color,
        theme = r$theme
      )
    }) |>
      shiny::bindEvent(r$go_heatmap, r$theme)
    output$heatmap_legend <- plotly::renderPlotly({
      treemap_legend()
    }) |>
      shiny::bindEvent(r$go_heatmap, r$theme)
    # Flag when Calculate! is pressed, removes the help text even if plotly is
    # not rendered (due to validation error when all DFDR values are NULL)
    output$heatmap_ready <- shiny::renderText("Heatmap ready") |>
      shiny::bindEvent(r$go_heatmap)
    # Filters applied
    output$filter_list_adae <- shiny::renderText(r$filter_list_adae) |>
      shiny::bindEvent(r$go_heatmap)
    output$filter_list_adsl <- shiny::renderText(r$filter_list_adsl) |>
      shiny::bindEvent(r$go_heatmap)
  })
}
