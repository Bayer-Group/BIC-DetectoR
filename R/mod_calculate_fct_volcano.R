# VOLCANO PREP
#' Prepare adsl and adae for the volcano display
#' @param data Results data.
#' @param safety_variable The hierarchy level to display: "AEDECOD", "AEBODSYS",
#'  "MLG_label", "SMQ_NAME".
prepare_volcano <- function(
  data,
  safety_variable = c("AEDECOD", "AEBODSYS", "MLG_label", "SMQ_NAME")
) {
  safety_variable <- match.arg(safety_variable)
  # Get total counts (will be the size of the circle)
  totals <- data |>
    dplyr::count(.data[[safety_variable]], wt = .data$count, name = "total")
  data_with_totals <- data |>
    dplyr::select(
      tidyselect::all_of(safety_variable),
      tidyselect::any_of(c(
        "DFDR",
        "p_value_label",
        "p_adj_label",
        "DFDR_label"
      )),
      "rr",
      "rd",
      "p",
      "p_adj",
      "trta_detector",
      "count"
    ) |>
    dplyr::left_join(totals, by = safety_variable)
  # Store number of events in verum and comparator for display in hover menu
  data_distinct <- data_with_totals |>
    tidyr::pivot_wider(
      names_from = "trta_detector",
      values_from = "count"
    ) |>
    dplyr::rename(axis_var = safety_variable) |>
    dplyr::rename_with(tolower)
  data_distinct
}

#' Color volcano circles according to significance and effect
#' @param data Data prepared for volcano plot.
#' @param effect_measure Either "RR" or "RD".
#' @param alpha Significance level.
color_volcano <- function(
  data,
  effect_measure,
  alpha
) {
  if (effect_measure == "RR") {
    data <- data |>
      dplyr::mutate(
        legend = dplyr::case_when(
          .data$p >= alpha ~ "Non-significant",
          dplyr::between(.data$rr, 0.5, 2) ~ "Non-significant",
          .data$rr < 0.5 ~ "Favours verum",
          .data$rr > 2 ~ "Favours comparator"
        )
      )
  } else if (effect_measure == "RD") {
    data <- data |>
      dplyr::mutate(
        legend = dplyr::case_when(
          .data$p >= alpha ~ "Non-significant",
          .data$rd < 0 ~ "Favours verum",
          .data$rd > 0 ~ "Favours comparator"
        )
      )
  }
  data |>
    dplyr::mutate(
      color = dplyr::case_when(
        .data$legend == "Non-significant" ~ "white",
        .data$legend == "Favours verum" ~ "blue",
        .data$legend == "Favours comparator" ~ "red",
      )
    )
}
