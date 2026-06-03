#' Helper function to calculate the count of unique USUBJID by treatment arm
#' @param data A dataframe.
#' @param arm A treatment arm, can be "Verum" or "Comparator".
#' @param filter A variable name to filter not null values.
count_subjects <- function(
  data,
  arm = c("Verum", "Comparator"),
  filter = NULL
) {
  arm <- match.arg(arm)
  if (!is.null(filter)) {
    data <- data |>
      dplyr::filter(!is.na(.data[[filter]]))
  }
  count <- data |>
    dplyr::distinct(.data$USUBJID, .data$trta_detector) |>
    dplyr::count(.data$trta_detector) |>
    dplyr::filter(.data$trta_detector == arm) |>
    dplyr::pull("n")
  # When filtering out all records, count is stored as integer(0)
  if (length(count) == 0) {
    count <- 0
  }
  count
}

#' Calculate summary data to display in Dataset Information panel
#' @param data An unfiltered dataframe.
#' @param data_filtered A filtered dataframe.
calc_summary_data <- function(
  data,
  data_filtered
) {
  # Count number of subjects
  total_verum <- count_subjects(data, arm = "Verum")
  total_comparator <- count_subjects(data, arm = "Comparator")
  total_all <- total_verum + total_comparator

  # Count subjects with Adverse Events
  aes_verum <- count_subjects(data, arm = "Verum", filter = "AEDECOD")
  aes_comparator <- count_subjects(data, arm = "Comparator", filter = "AEDECOD")
  aes_all <- aes_verum + aes_comparator

  # Count filtered number of subjects
  total_verum_filtered <- count_subjects(data_filtered, arm = "Verum")
  total_comparator_filtered <- count_subjects(data_filtered, arm = "Comparator")
  total_all_filtered <- total_verum_filtered + total_comparator_filtered

  # Count filtered subjects with Adverse Events
  aes_verum_filtered <- count_subjects(
    data_filtered,
    arm = "Verum",
    filter = "AEDECOD"
  )
  aes_comparator_filtered <-
    count_subjects(data_filtered, arm = "Comparator", filter = "AEDECOD")
  aes_all_filtered <- aes_verum_filtered + aes_comparator_filtered

  list(
    total_verum = total_verum,
    total_comparator = total_comparator,
    total_all = total_all,
    aes_verum = aes_verum,
    aes_comparator = aes_comparator,
    aes_all = aes_all,
    total_verum_filtered = total_verum_filtered,
    total_comparator_filtered = total_comparator_filtered,
    total_all_filtered = total_all_filtered,
    aes_verum_filtered = aes_verum_filtered,
    aes_comparator_filtered = aes_comparator_filtered,
    aes_all_filtered = aes_all_filtered
  )
}
#' Display summary info
#' @param data_unfiltered Joined data before applying filters.
#' @param data_filtered Joined data after applying filters.
#' @param adae_unfiltered Full ADAE.
#' @param adae_filtered ADAE after applying filters.
#' @param verum_name Custom verum arm name.
#' @param comparator_name Custom comparator arm name.
display_info_summary <- function(
  data_unfiltered,
  data_filtered,
  adae_unfiltered,
  adae_filtered,
  verum_name,
  comparator_name
) {
  summ <- calc_summary_data(
    data = data_unfiltered,
    data_filtered = data_filtered
  )
  is_filtered <- nrow(data_filtered) < nrow(data_unfiltered)
  adae_unfiltered_rows <- nrow(adae_unfiltered)
  adae_filtered_rows <- nrow(adae_filtered)
  total_verum_comparator <- paste0(
    "(Total/",
    "<span class='verum-col'>",
    verum_name,
    "</span>/",
    "<span class='comparator-col'>",
    comparator_name,
    "</span>)"
  )
  shiny::HTML(
    paste0(
      "<p> Numbers expressed as ",
      total_verum_comparator,
      "<br><br>",
      "<strong>Number of subjects</strong><br>",
      "Total: ",
      summ$total_all,
      "/",
      "<span class='verum-col'>",
      summ$total_verum,
      "</span>/",
      "<span class='comparator-col'>",
      summ$total_comparator,
      "</span>",
      "<br>",
      ifelse(
        is_filtered,
        paste0(
          "Filtered: ",
          summ$total_all_filtered,
          "/",
          "<span class='verum-col'>",
          summ$total_verum_filtered,
          "</span>/",
          "<span class='comparator-col'>",
          summ$total_comparator_filtered,
          "</span><br>"
        ),
        ""
      ),
      "<br>",
      "<strong>Number of adverse events</strong><br>",
      "Total: ",
      adae_unfiltered_rows,
      ifelse(
        is_filtered,
        paste0(" (filtered: ", adae_filtered_rows, ")"),
        ""
      ),
      "<br><br>",
      "<strong>Number of subjects with adverse events</strong><br>",
      "Total: ",
      summ$aes_all,
      "/",
      "<span class='verum-col'>",
      summ$aes_verum,
      "</span>/",
      "<span class='comparator-col'>",
      summ$aes_comparator,
      "</span>",
      "<br>",
      ifelse(
        is_filtered,
        paste0(
          "Filtered: ",
          summ$aes_all_filtered,
          "/",
          "<span class='verum-col'>",
          summ$aes_verum_filtered,
          "</span>/",
          "<span class='comparator-col'>",
          summ$aes_comparator_filtered,
          "</span>"
        ),
        ""
      ),
      "</p>"
    )
  )
}
