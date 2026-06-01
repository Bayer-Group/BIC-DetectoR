#' info UI Function
#'
#' @description A shiny Module.
#'
#' @inheritParams mod_upload_ui
#'
#' @noRd
#'
mod_info_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # Dataset Information ----
    shiny::div(
      class = "upload-middle",
      shiny::wellPanel(
        detector_pretty_toggle(
          id = ns("show_dataset_info"),
          label = "Dataset Information",
          icon = "info-circle"
        ),
        shiny::conditionalPanel(
          condition = "input.show_dataset_info",
          ns = ns,
          shiny::uiOutput(ns("dataset_info"))
        )
      )
    ),
    # Study Sites information ----
    shiny::div(
      class = "upload-middle",
      shiny::wellPanel(
        detector_pretty_toggle(
          id = ns("show_sites_info"),
          label = "Study Sites Information",
          icon = "university"
        ),
        shiny::conditionalPanel(
          condition = "input.show_sites_info",
          ns = ns,
          shiny::textOutput(ns("sites_info"))
        )
      )
    )
  )
}

#' info Server Functions
#' @inheritParams mod_upload_server
#' @noRd
mod_info_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    dataset_info <- shiny::eventReactive(
      c(r$unfiltered_data, r$filtered_data, r$adae_filtered),
      {
        validate_need(
          r$filtered_data,
          "No data to show! Please check the data filters."
        )
        display_info_summary(
          data_unfiltered = r$unfiltered_data,
          data_filtered = r$filtered_data,
          adae_unfiltered = r$adae_data,
          adae_filtered = r$adae_filtered,
          verum_name = r$verum_name,
          comparator_name = r$comparator_name
        )
      }
    )
    output$dataset_info <- shiny::renderUI(dataset_info()) |>
      shiny::bindEvent(r$unfiltered_data, r$filtered_data)
    ## Study site information ----
    sites_info <- shiny::eventReactive(c(r$unfiltered_data, r$filtered_data), {
      shiny::req(r$unfiltered_data, r$adae_data)
      validate_need(
        r$filtered_data,
        "No data to show! Please check the data filters."
      )
      if (!is.null(r$filtered_data)) {
        data <- r$filtered_data
      } else {
        data <- r$unfiltered_data
      }
      studies <- data |>
        dplyr::distinct(.data$USUBJID, .data$STUDYID) |>
        dplyr::count(.data$STUDYID) |>
        dplyr::mutate(text = paste0(.data$STUDYID, " (n=", .data$n, ")"))
      paste0(
        "Number of studies: ",
        nrow(studies),
        ifelse(
          nrow(studies) > 1,
          paste(
            " with STUDYID (number of subjects): ",
            paste(studies$text, collapse = ", ")
          ),
          ""
        )
      )
    })
    output$sites_info <- shiny::renderText(sites_info()) |>
      shiny::bindEvent(r$unfiltered_data, r$filtered_data)
  })
}
