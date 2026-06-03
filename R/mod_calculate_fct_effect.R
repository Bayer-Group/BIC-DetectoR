#' Perform fisher test and calculate the relative risk and risk difference based
#'  in incidence proportions
#'
#'@param data Data set. Must have variables trta_detector, count, no_count,
#' prop, big_n
#'@param alternative Either one or two sided test.
#'@param alpha Significance level.
#'@param variable Target column.
#'@return output A tibble with the results.
get_fisher_rr_proportions <- function(
  data,
  alternative = "two.sided",
  alpha = 0.05,
  variable
) {
  checkmate::assert_data_frame(data, min.rows = 1)

  # Check for errors in data ----
  needed_columns <- c("trta_detector", "count", "no_count", "prop", "big_n")
  assert_columns(data, needed_columns)
  checkmate::assert_numeric(data$count, any.missing = FALSE)
  checkmate::assert_numeric(data$no_count, any.missing = FALSE)
  checkmate::assert_numeric(data$prop, any.missing = FALSE)
  checkmate::assert_numeric(data$big_n, any.missing = FALSE)
  checkmate::assert_factor(
    data$trta_detector,
    any.missing = FALSE,
    n.levels = 2,
    levels = c("Verum", "Comparator")
  )
  assertthat::assert_that(
    nrow(data) == length(unique(data[[variable]])) * 2,
    msg = "There must be two rows per adverse event!"
  )
  assertthat::assert_that(
    nrow(data |> dplyr::filter(.data$trta_detector == "Verum")) ==
      nrow(data |> dplyr::filter(.data$trta_detector == "Comparator")),
    msg = "There must be one row per Verum, one per Comparator!"
  )
  # Prepare data in wide format, one row per adverse event ----
  data_mod <- tidyr::pivot_wider(
    data = data,
    id_cols = tidyselect::all_of(variable),
    names_from = "trta_detector",
    values_from = c("count", "no_count", "prop", "big_n")
  ) |>
    # lowercase names of variables except safety variable
    dplyr::rename_with(tolower, -tidyselect::all_of(variable))
  checkmate::assert_data_frame(data_mod, any.missing = FALSE, min.rows = 1)

  ## Check column names are OK ----
  req_columns <- c(
    "count_verum",
    "big_n_verum",
    "count_comparator",
    "big_n_comparator"
  )
  assert_columns(data_mod, req_columns)

  data_mod <- data_mod |>
    dplyr::mutate(
      # Calculates RR (zero-corrected) and CI
      rr = get_rr(
        .data$count_verum,
        .data$big_n_verum,
        .data$count_comparator,
        .data$big_n_comparator,
        mode = "proportions",
        alpha = alpha
      ),
      # Calculates RD in % (uncorrected) and CI
      rd = 100 *
        get_rd(
          .data$count_verum,
          .data$big_n_verum,
          .data$count_comparator,
          .data$big_n_comparator,
          mode = "proportions",
          alpha = alpha
        ),
    ) |>
    tidyr::unnest_wider("rr") |>
    tidyr::unnest_wider("rd") |>
    # Calculates p-values from Fisher's exact test
    dplyr::rowwise() |>
    dplyr::mutate(
      p = get_fisher_p_value(
        .data$count_verum,
        .data$no_count_verum,
        .data$count_comparator,
        .data$no_count_comparator,
        alternative = alternative
      ),
      p_value_label = format_p_values(.data$p)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      dplyr::all_of(variable),
      dplyr::starts_with("prop_"),
      dplyr::starts_with("rr"),
      dplyr::starts_with("rd"),
      "p",
      "p_value_label"
    ) |>
    dplyr::rename(
      "prob2" = "prop_verum",
      "prob1" = "prop_comparator"
    )
  data_mod
}

#' Perform fisher test and calculate the relative risk and risk difference based
#'  on incidence rates
#'
#'@param data Data set. Must have variables trta_detector, count, no_count,
#' prop, big_n, pattime
#'@param alternative Either one or two sided test.
#'@param alpha Significance level.
#'@param variable Target column.
#'@return output A tibble with the results.
get_fisher_rr_rates <- function(
  data,
  alternative = "two.sided",
  alpha = 0.05,
  variable
) {
  # Check for errors in data
  req_columns <- c(
    "trta_detector",
    "count",
    "no_count",
    "prop",
    "big_n",
    "pattime"
  )
  assert_columns(data, req_columns)
  checkmate::assert_numeric(data$count, any.missing = FALSE)
  checkmate::assert_numeric(data$no_count, any.missing = FALSE)
  checkmate::assert_numeric(data$prop, any.missing = FALSE)
  checkmate::assert_numeric(data$big_n, any.missing = FALSE)
  checkmate::assert_numeric(data$pattime, any.missing = FALSE)
  assertthat::assert_that(
    nrow(data) == length(unique(data[[variable]])) * 2,
    msg = "There must be two rows per adverse event!"
  )
  # Prepare data in wide format, one row per adverse event
  data_mod <- tidyr::pivot_wider(
    data = data,
    id_cols = tidyselect::all_of(variable),
    names_from = "trta_detector",
    values_from = c("count", "no_count", "prop", "big_n", "pattime")
  ) |>
    # lowercase names of variables except safety variable
    dplyr::rename_with(tolower, -tidyselect::all_of(variable)) |>
    dplyr::mutate(
      # Calculates RR (zero-corrected) and CI
      rr = get_rr(
        .data$count_verum,
        .data$pattime_verum,
        .data$count_comparator,
        .data$pattime_comparator,
        mode = "incidence rates",
        alpha = alpha
      ),
      # Calculates RD in cases/100 patient-years (uncorrected) and CI
      rd = 100 *
        365.26 *
        get_rd(
          .data$count_verum,
          .data$pattime_verum,
          .data$count_comparator,
          .data$pattime_comparator,
          mode = "incidence rates",
          alpha = alpha
        ),
    ) |>
    tidyr::unnest_wider("rr") |>
    tidyr::unnest_wider("rd") |>

    # Calculates p-values from Fisher's exact test
    dplyr::rowwise() |>
    dplyr::mutate(
      p = get_fisher_p_value(
        .data$count_verum,
        .data$pattime_verum,
        .data$count_comparator,
        .data$pattime_comparator,
        alternative = alternative
      ),
      p_value_label = format_p_values(.data$p)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      dplyr::all_of(variable),
      dplyr::starts_with("prop_"),
      dplyr::starts_with("rr"),
      dplyr::starts_with("rd"),
      "p",
      "p_value_label",
    ) |>
    dplyr::rename(
      "prob2" = "prop_verum",
      "prob1" = "prop_comparator"
    )

  data_mod
}

#' Calculates Risk Difference and Confidence Intervals for proportions or
#' incidence rates
#'
#' @description Calculates Risk Difference (RD) and Confidence Intervals (CI),
#' for proportions or incidence rates of Adverse Events. It calls
#' DescTools::BinomDiffCI().
#'
#' @param x1 Numeric, number of cases in verum group
#' @param n1 Numeric, patient-time at risk (incidence rates) or number of
#' patients (proportions) in verum group
#' @param x2 Numeric, number of cases in verum group
#' @param n2 Numeric, patient-time at risk (incidence rates) or number of
#' patients (proportions) in comparison group
#' @param method Character, by default "wald". Any of the methods of CI
#' calculation accepted by DescTools::BinomDiffCI()
#' @param mode Character, "proportions" or "incidence rates"
#' @param alpha Numeric, significance level
#'
#' @returns A tibble, with columns named "rd" (estimate), "rd_lcl" (lower CI
#' limit), and "rd_ucl" (upper CI limit)
get_rd <- function(
  x1,
  n1,
  x2,
  n2,
  method = "wald",
  mode = c("proportions", "incidence rates"),
  alpha
) {
  mode <- match.arg(mode)
  conf_level <- 1 - alpha

  rd <- tidyr::as_tibble(DescTools::BinomDiffCI(
    x1,
    n1,
    x2,
    n2,
    method = method,
    sides = "two.sided",
    conf.level = conf_level
  ))
  colnames(rd) <- c("rd", "rd_lcl", "rd_ucl")
  rd
}

#' Calculates Risk Ratio and Confidence Intervals for proportions or incidence
#' rates
#'
#' @description Calculates Risk Ratio (RR) and Confidence Intervals (CI), for
#' proportions or incidence rates of Adverse Events, using a zero-value
#' correction. It calls DescTools::BinomRatioCI().
#'
#' @param x1 Numeric, number of cases in verum group
#' @param n1 Numeric, patient-time at risk (incidence rates) or number of
#' patients (proportions) in verum group
#' @param x2 Numeric, number of cases in verum group
#' @param n2 Numeric, patient-time at risk (incidence rates) or number of
#' patients (proportions) in comparison group
#' @param method Character, by default "katz.log". Any of the methods of CI
#' calculation accepted by DescTools::BinomRatioCI()
#' @param mode Character, "proportions" or "incidence rates"
#' @param alpha Numeric, significance level
#'
#' @returns A tibble, with columns named "rr" (estimate), "rr_lcl" (lower CI
#' limit), and "rr_ucl" (upper CI limit)
get_rr <- function(
  x1,
  n1,
  x2,
  n2,
  method = "katz.log",
  mode = c("proportions", "incidence rates"),
  alpha
) {
  mode <- match.arg(mode)
  conf_level <- 1 - alpha
  checkmate::assert_numeric(x1, any.missing = FALSE)
  checkmate::assert_numeric(n1, any.missing = FALSE)
  checkmate::assert_numeric(x2, any.missing = FALSE)
  checkmate::assert_numeric(n2, any.missing = FALSE)

  # adds 0.5 if any cell has value 0
  zero_value <- pmin(x1, n1, x2, n2) == 0
  if (any(zero_value)) {
    x1[zero_value] <- x1[zero_value] + 0.5
    x2[zero_value] <- x2[zero_value] + 0.5
    n1[zero_value] <- n1[zero_value] + 1
    n2[zero_value] <- n2[zero_value] + 1
  }
  rr <- tidyr::as_tibble(DescTools::BinomRatioCI(
    x1,
    n1,
    x2,
    n2,
    method = method,
    sides = "two.sided",
    conf.level = conf_level
  ))
  colnames(rr) <- c("rr", "rr_lcl", "rr_ucl")
  rr
}

#' Calculates Fisher's exact test p-values
#'
#' @description Obtains p-values from Fisher's exact test on a 2 x 2 table.
#'
#' @param a,b,c,d Numeric, each one of the variables to compare. In proportions,
#'  counts of cases and no-cases. In incidence rates, counts of cases and
#' patient-times.
#' @param alternative Character, indicates the alternative hypothesis and must
#' be "two.sided", "greater" or "less"
#'
#' @returns A numeric vector with p-values
get_fisher_p_value <- function(a, b, c, d, alternative) {
  stats::fisher.test(
    matrix(c(a, b, c, d), nrow = 2),
    alternative = alternative
  )$p.value
}
