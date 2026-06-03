#' Adjust p-values using Benjamini-Hochberg False Discovery Rate (FDR) method
#'
#' @param data A dataframe of results. Must include a column named "p", with
#' unadjusted p-values
#' @returns A dataframe with adjusted p-values, in a column named "p_adj"
#'
get_fdr_p_values <- function(
  data
) {
  results <- data |>
    c212::c212.BH.adjust.pvals() |>
    dplyr::mutate(
      p_adj_label = format_p_values(.data$p_adj)
    ) |>
    tidyr::as_tibble()
  results
}

#' Calculate the false discovery rate (this is an almost duplicate of
#' c212.BH.adjust.pvals())
#'
#'@param data The ADAE data set.
#'@return An ordered dataframe with FDR p-values
#'

get_fdr <- function(data) {
  if (is.null(data)) {
    print("NULL data")
    return(NULL)
  }
  if (is.character(data)) {
    file <- data
    data <- utils::read.table(file, header = TRUE, stringsAsFactors = FALSE)
  }
  if (!("p" %in% colnames(data))) {
    print("p column missing")
    return(NULL)
  }
  m <- nrow(data)
  if (is.null(m)) {
    print("No data found")
    return(NULL)
  }
  if (m == 0) {
    data[, "FDR"] <- numeric()
    return(data)
  }

  ordered_data <- data[order(data$p), , drop = FALSE]
  ordered_data$FDR <- rep(NA, m)
  ordered_data$FDR[m] <- ordered_data$p[m]
  if (m > 1) {
    for (j in (m - 1):1) {
      ordered_data$FDR[j] <- min(
        ordered_data$p[j] * m / j,
        ordered_data$FDR[j + 1]
      )
    }
  }
  ordered_data
}

#' Calculate the new DFDR adjusted p-values
#'
#' @param data A dataframe with "p", lower AE category ("AEDECOD" or
#' "MLG_label"), and higher AE category ("AEBODSYS" or "SOC_MLG")
#' @param variable A character, the lower AE category ("AEDECOD" or "MLG_label")
#' @param alpha Numeric, the significance level
#'
#' @returns A dataframe with the adjusted p-values in column "DFDR"
#'
get_new_dfdr <- function(
  data,
  variable = c("AEDECOD", "MLG_label"),
  alpha
) {
  variable <- match.arg(variable)

  if (variable == "AEDECOD") {
    higher_variable <- "AEBODSYS"
  } else if (variable == "MLG_label") {
    higher_variable <- "SOC_MLG"
  }

  # First step: Flag SOCs
  soc_dfdr <- data |>
    dplyr::group_by(.data[[higher_variable]]) |>
    tidyr::nest() |>
    dplyr::mutate(
      # Adjust p-values for PTs within each SOC; handle NULL or 0-row from get_fdr
      data = purrr::map(data, function(x) {
        out <- get_fdr(x)
        if (is.null(out) || nrow(out) == 0) {
          NA_real_
        } else {
          min(out$FDR, na.rm = TRUE)
        }
      })
    ) |>
    tidyr::unnest(data) |>
    dplyr::rename(p = data) |>
    get_fdr() |> # adjust p-values for SOCs
    dplyr::select(dplyr::all_of(higher_variable), "FDR") |>
    dplyr::filter(.data$FDR <= alpha) # Keep flagged SOCs

  # Second step: Flag AEs
  aes_dfdr <- data |>
    dplyr::filter(.data[[higher_variable]] %in% soc_dfdr[[higher_variable]]) |>
    get_fdr() |>
    dplyr::rename(DFDR = "FDR") |>
    dplyr::select(dplyr::all_of(variable), "DFDR")

  # Merge back and fill empty results
  data |>
    dplyr::left_join(aes_dfdr, by = variable) |>
    dplyr::mutate(
      DFDR_label = format_p_values(.data$DFDR)
    )
}

#' Count proportions of total AEs by AE category
#'
#' @param data A dataframe, with columns "variable" (AEDECOD or MLG_label),
#' "count", "big_n" and "p"
#' @param variable A character, either "AEDECOD" or "MLG_label"
#'
#' @returns A dataframe with total Count of events, and proportion of all events
#' (column "prop")
#'
count_total_proportions <- function(data, variable) {
  data |>
    dplyr::select(tidyselect::all_of(variable), "count", "big_n", "p") |>
    dplyr::group_by(.data[[variable]]) |>
    dplyr::summarise(
      Count = sum(.data$count),
      Total = sum(.data$big_n),
      p = as.numeric(.data$p[1]),
      .groups = "drop"
    ) |>
    dplyr::mutate(prop = .data$Count / .data$Total)
}

#'Calculate stratified relative risk and risk differences
#'
#'@param comb_data adsl and adae data set combined
#'@param variable Safety variable.
#'@param effect_measure Effect measure, either "RR" or "RD".
#'@param alpha Significance level.
#'@param stratify_variable Stratification variable.
#'
#'@return results
get_stratified_rr_rd <- function(
  comb_data,
  variable,
  effect_measure = c("RD", "RR"),
  alpha,
  stratify_variable
) {
  effect_measure <- match.arg(effect_measure)
  level <- 100 * (1 - alpha)
  assert_columns(
    comb_data,
    c(variable, stratify_variable, "USUBJID", "trta_detector")
  )
  comb_data <- comb_data |>
    # Convert stratification variable to factor to allow 0 counts
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(c(variable, stratify_variable, "trta_detector")),
        factor
      )
    )
  # Calculation of number of subjects within stratification variable and arm
  n_stratified <- comb_data |>
    # Count unique subjects
    dplyr::distinct(.data$USUBJID, .keep_all = TRUE) |>
    dplyr::count(.data$trta_detector, .data[[stratify_variable]])
  # Counting events
  events_stratified <- comb_data |>
    # Keep one event per subject
    dplyr::distinct(
      .data[[variable]],
      .data$USUBJID,
      .keep_all = TRUE
    ) |>
    # Count how many events per stratification_variable and treatment arm
    dplyr::count(
      .data[[stratify_variable]],
      .data[[variable]],
      .data$trta_detector,
      name = "events",
      .drop = FALSE
    )
  # Join numerators and denominators
  count_stratified <- dplyr::left_join(
    events_stratified,
    n_stratified,
    by = c(stratify_variable, "trta_detector")
  ) |>
    dplyr::mutate(no_event = .data$n - .data$events) |>
    dplyr::group_by(.data[[variable]]) |>
    # Convert results for each AE category into a nested tibble
    tidyr::nest() |>
    dplyr::ungroup()
  # Apply function to each nested tibble
  results <- count_stratified |>
    dplyr::mutate(
      data = purrr::map(
        .data$data,
        \(x) {
          calculate_stratified_incidence(
            x,
            effect_measure = effect_measure,
            level = level,
            stratify_variable = stratify_variable
          )
        },
        .progress = TRUE
      )
    ) |>
    tidyr::unnest("data") |>
    dplyr::mutate(
      p_value_label = format_p_values(.data$p)
    )
  results
}

#' Calculate the study-size adjusted incidence rates
#'
#'@param split_data Data set with Counts.
#'@param effect_measure Effect measure, either "RR" or "RD".
#'@param level Significance level (e.g., 95).
#'@param stratify_variable Stratification variable.
#'
#'@return A one-row tibble with weighted proportions, relative risk,
#' risk differences, confidence intervals and p-values.
calculate_stratified_incidence <- function(
  split_data,
  effect_measure = c("RD", "RR"),
  level,
  stratify_variable
) {
  effect_measure <- match.arg(effect_measure)
  # Calculate weighted probabilities for stratification variable and arm
  weigthed_freqs <- split_data |>
    dplyr::count(.data[[stratify_variable]], wt = .data$n) |>
    dplyr::mutate(freq = .data$n / sum(.data$n)) |>
    dplyr::select(tidyselect::all_of(stratify_variable), "freq")
  split_data <- split_data |>
    dplyr::left_join(weigthed_freqs, by = stratify_variable) |>
    dplyr::mutate(
      prop_raw = .data$events / .data$n,
      prop_weighted = .data$prop_raw * .data$freq
    )
  # Vectorized filtering
  is_verum <- split_data$trta_detector == "Verum"
  is_comparator <- split_data$trta_detector == "Comparator"
  prob1 <- sum(split_data$prop_weighted[is_comparator])
  prob2 <- sum(split_data$prop_weighted[is_verum])
  # Get 2x2 count data - vectorized extraction
  ai <- split_data$events[is_verum]
  bi <- split_data$no_event[is_verum]
  n1i <- split_data$n[is_verum]
  ci <- split_data$events[is_comparator]
  di <- split_data$no_event[is_comparator]
  n2i <- split_data$n[is_comparator]
  # Calculate p-values and confidence intervals with Mantel-Haenszel method
  if (effect_measure == "RD") {
    results <- metafor::rma.mh(
      ai,
      bi,
      ci,
      di,
      n1i,
      n2i,
      measure = "RD",
      add = 0,
      drop00 = FALSE,
      level = level,
      verbose = FALSE
    )
  } else if (effect_measure == "RR") {
    results <- metafor::rma.mh(
      ai = ai + 0.5,
      bi = bi + 0.5,
      ci = ci + 0.5,
      di = di + 0.5,
      n1i = n1i + 1,
      n2i = n2i + 1,
      measure = "RR",
      add = 0,
      level = level,
      verbose = FALSE
    )
  }
  results_ci <- stats::confint(results)$fixed
  if (effect_measure == "RR") {
    results_ci <- exp(results_ci)
  }
  # Tibble of results (one row per nested tibble)
  effect_variable <- tolower(effect_measure) # "rd" or "rr"
  lcl_variable <- paste0(effect_variable, "_lcl")
  ucl_variable <- paste0(effect_variable, "_ucl")
  res <- tibble::tibble(
    "prob1" = prob1,
    "prob2" = prob2,
    {{ effect_variable }} := results_ci[1],
    {{ lcl_variable }} := results_ci[2],
    {{ ucl_variable }} := results_ci[3],
    "p" = results$pval
  )
  res
}
