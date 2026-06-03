#' table UI Function
#'
#' @inheritParams mod_upload_ui
mod_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Select main options panel
    mod_calculate_ui("calculate_table", calculate_mode = "table"),
    # Table panel ----
    shiny::wellPanel(
      shiny::conditionalPanel(
        condition = "!output.table",
        ns = ns,
        shiny::span(
          shiny::icon("circle-info"),
          "Click the Calculate! button to display the table."
        )
      ),
      DT::DTOutput(ns("table"))
    ),
    shiny::br(),
    shiny::conditionalPanel(
      condition = "output.table",
      ns = ns,
      shiny::wellPanel(
        shiny::span(shiny::icon("filter"), "Filters applied:"),
        shiny::textOutput(ns("filter_list_adae")),
        shiny::textOutput(ns("filter_list_adsl"))
      )
    )
  )
}

#' safety Server Function
#'
#' @inheritParams mod_upload_server
#'
mod_table_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    table <- shiny::eventReactive(r$go_table, {
      shiny::req(
        r$table_data,
        r$table_variable,
        r$alpha,
        r$verum_name,
        r$comparator_name,
        r$adjustment_method
      )
      validate_need(
        nrow(r$table_data) > 0,
        "Filtered data has zero rows! Please check the active filters."
      )
      table_variable <- r$table_variable
      adjusted_p <- ifelse(
        r$adjustment_method == "FDR",
        "p_adj_label",
        "DFDR_label"
      )
      confidence <- (1 - r$alpha) * 100
      rd_text <- paste0("RD ", confidence, "% CI")
      rr_text <- paste0("RR ", confidence, "% CI")
      table <- r$table_data |>
        dplyr::select(
          tidyselect::all_of(table_variable),
          tidyselect::all_of(adjusted_p),
          "trta_detector",
          "count",
          "prop",
          "rd",
          "rd_lcl",
          "rd_ucl",
          "rr",
          "rr_lcl",
          "rr_ucl",
          "p_value_label"
        ) |>
        dplyr::mutate_if(is.numeric, \(x) format_decimals(x, 4)) |>
        dplyr::mutate(
          trta_detector = dplyr::case_when(
            trta_detector == "Verum" ~ r$verum_name,
            trta_detector == "Comparator" ~ r$comparator_name
          ),
          rd_ci = stringr::str_glue("[{rd_lcl}, {rd_ucl}]"),
          rr_ci = stringr::str_glue("[{rr_lcl}, {rr_ucl}]")
        ) |>
        dplyr::select(
          "Category" = table_variable,
          "Treatment" = "trta_detector",
          "Events" = "count",
          "Incidence" = "prop",
          "Risk difference" = "rd",
          {{ rd_text }} := "rd_ci",
          "Relative Risk" = "rr",
          {{ rr_text }} := "rr_ci",
          "adjusted p-value" = adjusted_p,
          "unadjusted p-value" = "p_value_label"
        )

      # Data table call and formatting ----
      DT::datatable(
        table,
        extensions = "Buttons",
        escape = FALSE,
        options = list(
          scrollX = TRUE, # Allows horizontal scrolling
          dom = "Brtip",
          buttons = c("copy", "print", "pageLength", I("colvis")),
          lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "All")),
          pageLength = 10
        ),
        class = "cell-border stripe",
        rownames = FALSE,
        caption = "Table of calculated results",
        filter = "top"
      ) |>
        DT::formatStyle(
          1,
          target = "row"
        )
    })
    output$table <- DT::renderDT({
      table()
    })
    # Filters applied
    output$filter_list_adae <- shiny::renderText(r$filter_list_adae) |>
      shiny::bindEvent(r$go_table)
    output$filter_list_adsl <- shiny::renderText(r$filter_list_adsl) |>
      shiny::bindEvent(r$go_table)
  })
}
