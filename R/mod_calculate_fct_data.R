#' Calculate all results for plotting
#'
#'@param joint_data Joined and filtered ADSL-ADAE data set with MedDRA data.
#'@param adsl_filtered_data A dataframe with ADSL after filtering.
#'@param variable A character, the variable name that will be shown in the plot
#'  ("AEDECOD", "MLG_label", "SMQ_NAME", "ocmq" or "AEBODSYS").
#'@param effect_measure Display option RR (relative risks) or RD (risk
#' differences).
#'@param adjustment Adjustment variable FDR or DFDR.
#'@param order_by Ordered by "p-value" or "effect"
#'@param study_strat A character, study stratification variable ("None" if
#'  none).
#'@param alternative A character string describing the alternative hypothesis
#'  (one or two sided).
#'@param alpha Alpha level of the test.
#'@param filter A character string if filter on adverse events should be
#'  performed.
#'@param frequency_measure Measurement used (proportions or incidence rates).
#'@param duration_mode Way of calculating incidence risks denominators (time at
#' risk),
#' can be "duration", "start_end_date" or "none".
#'@param ae_duration_variable Variables for calculation of time at risk for
#' incidence rates.
#'@param exposure_duration_variable Variables for calculation of time at risk
#' for incidence rates.
#'@param exposure_start_variable Variables for calculation of time at risk for
#' incidence rates.
#'@param exposure_end_variable Variables for calculation of time at risk for
#' incidence rates.
#'@param ae_start_variable Variables for calculation of time at risk for
#' incidence rates.
#'@return A dataframe with event counts, proportions/incidences, RR, RD,
#'  confidence intervals and p-values.
calculate_results <- function(
  joint_data,
  adsl_filtered_data,
  variable = c("AEDECOD", "MLG_label", "SMQ_NAME", "ocmq", "AEBODSYS"),
  effect_measure = c("RR", "RD"),
  adjustment = c("FDR", "DFDR"),
  order_by = c("p-value", "effect"),
  study_strat = "None",
  alternative = c("two.sided", "less"),
  alpha = 0.05,
  filter = c("no_method", "one_percent", "min"),
  frequency_measure = c("proportions", "incidence rates"),
  duration_mode = c("none", "duration", "start_end_date"),
  ae_duration_variable = NULL,
  exposure_duration_variable = NULL,
  exposure_start_variable = NULL,
  exposure_end_variable = NULL,
  ae_start_variable = NULL
) {
  logger::log_info("++++ calculate_results started ++++")
  variable <- match.arg(variable)
  effect_measure <- match.arg(effect_measure)
  adjustment <- match.arg(adjustment)
  alternative <- match.arg(alternative)
  filter <- match.arg(filter)
  frequency_measure <- match.arg(frequency_measure)
  duration_mode <- match.arg(duration_mode)
  assert_columns(joint_data, req_columns = variable)
  # Filter out empty or NA variable values
  comb_data <- filter_empty_variable(joint_data, variable)
  # Get big N denominators for verum and comparison
  big_n <- get_big_n(
    adsl_filtered_data,
    frequency_measure,
    duration_mode = duration_mode,
    exposure_duration_variable = exposure_duration_variable,
    exposure_start_variable = exposure_start_variable,
    exposure_end_variable = exposure_end_variable
  )
  # Calculation of RR / RD and p-values based on the condition of variable
  proportions_data <- comb_data |>
    get_count_proportions(
      big_n = big_n,
      variable = variable,
      frequency_measure = frequency_measure,
      duration_mode = duration_mode,
      ae_duration_variable = ae_duration_variable,
      exposure_duration_variable = exposure_duration_variable,
      exposure_start_variable = exposure_start_variable,
      exposure_end_variable = exposure_end_variable,
      ae_start_variable = ae_start_variable
    )
  # Add filters of minimum count of AEs
  filtered_data <- proportions_data |>
    filter_minimum_aes(
      variable = variable,
      big_n = big_n,
      method = filter,
      alternative = alternative,
      alpha = alpha
    )
  # Check that we still have rows left
  if (nrow(filtered_data) == 0) {
    # Early return of an empty tibble
    return(tibble::tibble())
  }
  # study_strat {FALSE/TRUE}
  if (study_strat == "None") {
    # Calculate counts and proportions
    if (frequency_measure == "proportions") {
      effect_data <- filtered_data |>
        get_fisher_rr_proportions(
          alternative = alternative,
          alpha = alpha,
          variable = variable
        )
    } else {
      effect_data <- filtered_data |>
        get_fisher_rr_rates(
          alternative = alternative,
          alpha = alpha,
          variable = variable
        )
    }
  } else if (study_strat != "None") {
    effect_data <- comb_data |>
      get_stratified_rr_rd(
        variable = variable,
        effect_measure = effect_measure,
        alpha = alpha,
        stratify_variable = study_strat
      ) |>
      dplyr::filter(.data[[variable]] %in% filtered_data[[variable]])
  }
  checkmate::assert_data_frame(effect_data, min.rows = 1)
  # Benjamini-Hochberg FDR procedure adjusted p-values ----
  effect_fdr_data <- effect_data |>
    get_fdr_p_values() |>
    flag_significant(
      alpha = alpha,
      adjustment = "FDR",
      effect_measure = effect_measure
    )
  # Join p-values with count data by treatment
  results_fdr_data <- filtered_data |>
    dplyr::left_join(effect_fdr_data, by = variable)
  # New double FDR ----
  if (adjustment == "DFDR" && variable %in% c("AEDECOD", "MLG_label")) {
    if (variable == "AEDECOD") {
      equivalence_pt_soc <- comb_data |>
        dplyr::distinct(.data$AEDECOD, .data$AEBODSYS)
    } else if (variable == "MLG_label") {
      equivalence_pt_soc <- comb_data |>
        dplyr::distinct(.data$MLG_label, .data$SOC_MLG)
    }
    # Remove OVERALL group, if present
    results_fdr_data <- results_fdr_data |>
      dplyr::filter(.data[[variable]] != "OVERALL")
    # Count the total cases by variable, and their total proportion
    aes_tier2 <- count_total_proportions(results_fdr_data, variable) |>
      dplyr::left_join(equivalence_pt_soc, by = variable)
    # Calculate new DFDR p-values and add color
    dfdr_data <- get_new_dfdr(aes_tier2, variable, alpha) |>
      dplyr::select(-"prop")
    results_dfdr_data <- results_fdr_data |>
      dplyr::left_join(dfdr_data, by = c(variable, "p")) |>
      flag_significant(
        adjustment = "DFDR",
        alpha = alpha,
        effect_measure = effect_measure
      )
    results_data <- results_dfdr_data
  } else {
    results_data <- results_fdr_data
  }
  checkmate::assert_data_frame(results_data, min.rows = 1)
  # Add hierarchies back for heatmap ----
  if (variable == "AEDECOD") {
    # Check in which scenario we are
    if ("MLG_label" %in% colnames(joint_data)) {
      # PT/MLG/SOC_MLG and PT/HLT/HLGT/SOC
      equivalence <- joint_data |>
        dplyr::distinct(
          .data$AEDECOD,
          .data$MT_HLT,
          .data$MT_HLGT,
          .data$AEBODSYS,
          .data$MLG_label,
          .data$SOC_MLG
        )
    } else {
      # PTs and SOCs
      equivalence <- joint_data |>
        dplyr::distinct(.data$AEDECOD, .data$AEBODSYS)
    }
    # Avoid duplicated AEBODSYS.x and AEBOSYS.y when joining
    if ("AEBODSYS" %in% colnames(results_data)) {
      join_by_variables <- c("AEDECOD", "AEBODSYS")
    } else {
      join_by_variables <- "AEDECOD"
    }
    results <- results_data |>
      dplyr::left_join(equivalence, by = join_by_variables)
  } else {
    results <- results_data
  }
  logger::log_info("---- calculate_results finished ----")
  results
}

#' Helper for filtering out empty values and convert to factor
#' @param data A dataframe
#' @param variable A character variable
filter_empty_variable <- function(data, variable) {
  data |>
    dplyr::filter(
      !is.na(.data[[variable]]),
      .data[[variable]] != ""
    ) |>
    # Convert character variables to factors to allow counting zero events
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(variable),
        factor
      )
    )
}

#' Filter the minimum count of AEs
#'
#' Takes a dataframe of adverse events (AEs) and filters it
#'  according to a minimum count criteria: "one_percent" (incidence proportion
#'  >=1\%) or "min" (minimum number of events in Verum arm).
#'
#' @param data A dataframe, the combined data of AEs.
#' @param variable A character, the safety variable used to group AEs.
#' @param big_n A dataframe, the summarised data with number of subjects per
#'  treatment arm.
#' @param method A character, the method of filtering: "no_method",
#'  "one_percent" or "min". "min" calls find_min_event()
#' @param alternative A character describing the alternative hypothesis for
#'  "min" method: "two.sided" or "less" (one-sided)
#' @param alpha Numeric, alpha level of the test for "min" method.
#'
#' @returns A dataframe of filtered adverse events.
filter_minimum_aes <- function(
  data,
  variable,
  big_n,
  method = c("no_method", "one_percent", "min"),
  alternative = c("two.sided", "less"),
  alpha
) {
  filter <- match.arg(method)
  alternative <- match.arg(alternative)
  if (filter == "no_method") {
    data
  } else if (filter == "one_percent") {
    assert_columns(data, c(variable, "count", "big_n"))
    data <- data |>
      # Calculate the % of total AEs (both arms)
      dplyr::group_by(.data[[variable]]) |>
      dplyr::mutate(
        sum_count = sum(.data$count, na.rm = TRUE),
        sum_big_n = sum(.data$big_n, na.rm = TRUE),
        total_prop = dplyr::if_else(
          .data$sum_big_n > 0,
          .data$sum_count / .data$sum_big_n,
          NA_real_
        )
      ) |>
      dplyr::ungroup() |>
      # Keep only >=1% of total
      dplyr::filter(.data$total_prop >= 0.01) |>
      dplyr::select(-c("sum_count", "sum_big_n", "total_prop"))
  } else if (filter == "min") {
    assert_columns(data, c(variable, "trta_detector", "count", "big_n"))
    # Obtain the number of subjects in Verum group
    n_verum <- big_n |>
      dplyr::filter(.data$trta_detector == "Verum") |>
      dplyr::pull("big_n")
    # Obtain the number of subjects in Comparator group
    n_comparator <- big_n |>
      dplyr::filter(.data$trta_detector == "Comparator") |>
      dplyr::pull("big_n")
    min_events <- find_min_event(
      n1 = n_verum,
      n2 = n_comparator,
      alternative = alternative,
      alpha = alpha
    )
    # If no minimum exists (e.g. no event count is significant), skip filtering
    if (!is.finite(min_events) || length(min_events) == 0) {
      return(data)
    }
    # Filter which AEs in Verum have larger count than min_events
    filtered_aes <- data |>
      dplyr::filter(
        .data$trta_detector == "Verum",
        .data$count >= min_events
      ) |>
      dplyr::pull(variable)
    data <- data |>
      dplyr::filter(.data[[variable]] %in% filtered_aes)
  }
  data
}

#' Reorder safety variable levels to plot according to custom ordering
#'
#' @param data A results dataframe
#' @param variable A character, the lowest-level safety variable. "AEDECOD" or
#' "MLG_label"
#' @param order_by A character, either "effect" or "p-value"
#' @param effect_measure A character, either "RD" or "RR"
#' @param adjustment A character, either "FDR" or "DFDR"
#'
#' @returns A dataframe, ordered by the selected method
#'
reorder_levels <- function(
  data,
  variable,
  order_by,
  effect_measure,
  adjustment
) {
  effect_measure <- tolower(effect_measure)
  # Make sure that dataframe is ungrouped. Otherwise, factors only
  # have two levels
  data_ae <- data |>
    dplyr::ungroup() |>
    # Deselect OVERALL (will be added at the end)
    dplyr::filter(.data[[variable]] != "OVERALL")

  if (order_by == "effect") {
    data_reordered <- data_ae |>
      dplyr::arrange(
        dplyr::desc(.data[[effect_measure]]),
        .data$p,
        .data[[variable]]
      )
  } else if (adjustment == "FDR") {
    data_reordered <- data_ae |>
      dplyr::arrange(.data$p_adj, .data$p, .data[[variable]])
  } else if (adjustment == "DFDR") {
    data_reordered <- data_ae |>
      dplyr::arrange(.data$DFDR, .data$p, .data[[variable]])
  }

  # Get levels of the variable in the desired order
  vector_reordered <- data_reordered |>
    dplyr::distinct(.data[[variable]]) |>
    # Add back OVERALL level
    dplyr::add_row({{ variable }} := "OVERALL", .before = 1) |>
    dplyr::pull(.data[[variable]]) |>
    rev() # By default, ggplot2 plots factors in reverse order

  # Add those labels to variable axis_var (the one shown in plots in axis)
  res <- data |>
    dplyr::mutate(
      axis_var = factor(.data[[variable]], levels = vector_reordered)
    )
  assertthat::assert_that(sum(!is.na(res[[variable]])) > 0)
  assertthat::assert_that(sum(!is.na(res$axis_var)) > 0)
  res
}

#' A function to prepare data for plotting the double dot plots
#'
#' Sorts data by order of effect or significance.
#'
#'@param data Result data set from function calculate_results.
#'@param order_by p-value or effect.
#'@param adjustment FDR (False Discovery Rate) or DFDR (Double False Discovery
#'  Rate).
#'@param effect_measure RR (Relative Risk) or RD (Risk Difference).
#'@param number_aes Number of Adverse Events shown.
#'
#'@return An data object arranged and filtered, ready to plot
#'
arrange_data <- function(
  data,
  order_by = c("p-value", "effect"),
  adjustment = c("FDR", "DFDR"),
  effect_measure = c("RD", "RR"),
  number_aes
) {
  order_by <- match.arg(order_by)
  adjustment <- match.arg(adjustment)
  effect_measure <- tolower(match.arg(effect_measure)) # "rr" or "rd"
  number_aes <- number_aes * 2 # two rows per AE (verum and comparator)
  assertthat::assert_that(sum(!is.na(data$axis_var)) > 0)
  # Remove empty axis_var labels in SMQ/OCMQ view
  data <- data |>
    dplyr::filter(!is.na(.data$axis_var))

  # Extract OVERALL rows (to add them back later)
  data_overall <- data |>
    dplyr::filter(.data$axis_var == "OVERALL")
  data_aes <- data |>
    dplyr::filter(.data$axis_var != "OVERALL")

  if (order_by == "p-value") {
    var <- dplyr::case_when(
      adjustment == "FDR" ~ "p_adj",
      adjustment == "DFDR" ~ "DFDR"
    )
    data_aes <- data_aes |>
      # ordered by adjusted p_value, then unadjusted p-value
      dplyr::arrange(.data[[var]], .data$p)
  } else if (order_by == "effect") {
    data_aes <- data_aes |>
      # order by RD or RR, descending
      dplyr::arrange(dplyr::desc(.data[[effect_measure]]))
  }
  # select only the rows to show on plots
  data_arranged <- dplyr::bind_rows(
    # Add OVERALL back
    data_overall,
    data_aes |>
      dplyr::slice_head(n = number_aes)
  ) |>
    # Remove unused levels to restrict plotly output
    droplevels()
  data_arranged
}

#' Filter by AE type (Serious, Treatment-Emergent, etc.)
#' @param data Data to filter on.
#' @param type AE type.
#' @param treatment_emergent_variable TEAE flag variable, e.g. "TRETMFL".
#' @param treatment_emergent_value TEAE flag value, e.g. "Y".
#' @param serious_variable SAE flag variable, e.g. "AESER".
#' @param serious_value SAE flag value, e.g. "Y".
#' @param drug_related_variable Drug-related AE flag variable, e.g. "AEREL".
#' @param drug_related_value Drug-related AE flag value, e.g. "Y".
filter_ae_type <- function(
  data,
  type = c(
    "treatment_emergent",
    "serious_treatment_emergent",
    "drug_related_treatment_emergent",
    "serious_drug_related_treatment_emergent",
    "all",
    "serious",
    "drug_related",
    "serious_drug_related"
  ),
  treatment_emergent_variable,
  treatment_emergent_value,
  serious_variable,
  serious_value,
  drug_related_variable,
  drug_related_value
) {
  assert_columns(data, c(treatment_emergent_variable, serious_variable))
  filter_treatment_emergent <- stringr::str_detect(type, "treatment_emergent")
  filter_serious <- stringr::str_detect(type, "serious")
  filter_drug_related <- stringr::str_detect(type, "drug_related")
  data |>
    dplyr::filter(
      # Filter TEAEs
      dplyr::case_when(
        filter_treatment_emergent ~ .data[[treatment_emergent_variable]] ==
          treatment_emergent_value,
        .default = TRUE
      ),
      # Filter SAEs
      dplyr::case_when(
        filter_serious ~ .data[[serious_variable]] == serious_value,
        .default = TRUE
      ),
      # Filter Drug-related
      dplyr::case_when(
        filter_drug_related ~ .data[[drug_related_variable]] ==
          drug_related_value,
        .default = TRUE
      ),
    )
}
