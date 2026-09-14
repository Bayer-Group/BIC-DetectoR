### FUNCTIONS TO COUNT EVENTS, PROPORTIONS AND INCIDENCE RATES ###

#' Get the denominator counts (N for arms) for proportions or incidence rates
#' @param adsl_filtered_data ADSL with filters applied
#' @param frequency_measure Either "proportions" or "incidence rates"
#' @param duration_mode Way of calculating incidence risks time at risk,
#' can be "duration", "start_end_date" or "none".
#' @param exposure_duration_variable Variables for calculation of time at risk
#' for incidence rates.
#' @param exposure_start_variable Variables for calculation of time at risk for
#' incidence rates.
#' @param exposure_end_variable Variables for calculation of time at risk for
#' incidence rates.
#' @returns A tibble with columns trta_detector, big_n (number of subjects),
#' big_exp (total duration of exposure, only if incidence rates)

get_big_n <- function(
  adsl_filtered_data,
  frequency_measure = c("proportions", "incidence rates"),
  duration_mode,
  exposure_duration_variable = NULL,
  exposure_start_variable = NULL,
  exposure_end_variable = NULL
) {
  frequency_measure <- match.arg(frequency_measure)
  if (frequency_measure == "proportions") {
    assert_columns(adsl_filtered_data, "trta_detector")
    big_n <- adsl_filtered_data |>
      dplyr::count(.data$trta_detector, name = "big_n", .drop = FALSE)
  } else if (frequency_measure == "incidence rates") {
    # Logic for incidence rates -----
    assertthat::assert_that(duration_mode %in% c("duration", "start_end_date"))
    if (duration_mode == "start_end_date") {
      assert_columns(
        adsl_filtered_data,
        c(
          "USUBJID",
          "trta_detector",
          exposure_start_variable,
          exposure_end_variable
        )
      )
      # Add duration variable
      adsl_filtered_data <- adsl_filtered_data |>
        dplyr::mutate(
          exposure_duration = as.numeric(
            as.Date(.data[[exposure_end_variable]]) -
              as.Date(.data[[exposure_start_variable]])
          ) +
            1
        )
      exposure_duration_variable <- "exposure_duration"
    }
    assert_columns(
      adsl_filtered_data,
      c(
        "USUBJID",
        "trta_detector",
        exposure_duration_variable
      )
    )
    # Count total number of participants and duration exposure
    big_n <- adsl_filtered_data |>
      dplyr::select(
        dplyr::all_of(c("USUBJID", "trta_detector", exposure_duration_variable))
      ) |>
      dplyr::group_by(.data$trta_detector, .drop = FALSE) |>
      dplyr::summarise(
        big_n = dplyr::n(), # number of participants
        # total duration of exposure
        big_exp = sum(.data[[exposure_duration_variable]], na.rm = TRUE),
        .groups = "drop"
      )
  }
  big_n
}

#' Count the AE events per treatment arm and AE category
#' @param data Combined ADAE-ADSL data
#' @param variable AE group category
#' @param overall Whether to calculate events per AE category, or overall
count_ae_events <- function(data, variable, overall = FALSE) {
  # Remove ADSL rows without adverse events
  data_filtered <- data |> dplyr::filter(!is.na(.data[[variable]]))
  if (!overall) {
    data_counts <- data_filtered |>
      # Keep unique AEs per subject (we only count each category once)
      dplyr::distinct(
        .data[[variable]],
        .data$USUBJID,
        .data$trta_detector
      ) |>
      # Count the number of participants with AEs per treatment group
      dplyr::count(
        .data[[variable]],
        .data$trta_detector,
        name = "count",
        .drop = FALSE # for counting 0 events
      )
  } else if (overall) {
    data_counts <- data_filtered |>
      # Count number of subjects with any AE, per treatment arm
      dplyr::distinct(
        .data$USUBJID,
        .data$trta_detector
      ) |>
      dplyr::count(
        .data$trta_detector,
        name = "count",
        .drop = FALSE # for counting 0 events
      ) |>
      dplyr::mutate(
        {{ variable }} := "OVERALL"
      )
  }
  data_counts
}

#' Join denominators
#' @param data_events Count of events
#' @param data_denominators Count of denominators
join_denominators <- function(data_events, data_denominators) {
  dplyr::left_join(data_events, data_denominators, by = "trta_detector")
}

#' Add proportions
#' @param data Data with events ("count") and denominators ("big_n")
add_proportions <- function(data) {
  data |>
    dplyr::mutate(
      no_count = .data$big_n - .data$count,
      prop = .data$count / .data$big_n
    )
}

# Get proportions data
#' @inheritParams count_ae_events data variable overall
#' @inheritParams join_denominators data_denominators
get_proportions_data <- function(data, variable, overall, data_denominators) {
  assert_columns(data, c(variable, "USUBJID", "trta_detector"))
  assert_columns(data_denominators, c("trta_detector", "big_n"))
  res <- data |>
    count_ae_events(variable, overall) |>
    join_denominators(data_denominators) |>
    add_proportions()
  res
}

#' Add duration variables
#' @param data Combined ADAE-ADSL data
#' @inheritParams calculate_results
add_durations <- function(
  data,
  exposure_end_variable,
  exposure_start_variable,
  ae_start_variable
) {
  # Check if the required variables exist
  assert_columns(
    data,
    c(
      ae_start_variable,
      exposure_start_variable,
      exposure_end_variable
    )
  )
  # Calculate durations
  res <- data |>
    dplyr::mutate(
      exposure_duration = as.numeric(
        as.Date(.data[[exposure_end_variable]]) -
          as.Date(.data[[exposure_start_variable]])
      ) +
        1,
      ae_duration = as.numeric(
        as.Date(.data[[ae_start_variable]]) -
          as.Date(.data[[exposure_start_variable]])
      ) +
        1
    )
  # Catch if there is an error with dates (submitting relative day instead of date, for example)
  # It results in all duration values being negative
  negative_exposure <- sum(res$exposure_duration < 0, na.rm = TRUE)
  if (negative_exposure > 0) {
    logger::log_error(
      "Negative values in exposure duration (n: {negative_exposure}). 
          Please check study start and study end variables, they should be dates."
    )
    rlang::abort(
      "Negative values in exposure duration. 
          Please check study start and study end variables, they should be dates."
    )
  }
  negative_time_to_ae <- sum(res$ae_duration < 0, na.rm = TRUE)
  if (negative_time_to_ae > 0) {
    logger::log_error(
      "Negative values in time to Advere Event onset (n: {negative_time_to_ae}). 
          Please check AE start and study start variables, they should be dates."
    )
    rlang::abort(
      "Negative values in time to Advere Event onset. 
          Please check AE start and study start variables, they should be dates."
    )
  }
  res
}

#' Calculate numerators of incidence rates
#' @param data ADAE-ADSL data with duration variables
#' @param overall Whether to calculate events per AE category, or overall
#' @inheritParams calculate_results
add_incidence_numerators <- function(
  data,
  variable,
  ae_duration_variable,
  exposure_duration_variable,
  overall = FALSE
) {
  # Previous checks
  assert_columns(
    data,
    c(
      variable,
      "USUBJID",
      "trta_detector",
      ae_duration_variable,
      exposure_duration_variable
    )
  )
  data_filtered <- data |>
    # Remove ADSL rows without adverse event
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::select(
      dplyr::all_of(c(
        variable,
        ae_duration_variable,
        exposure_duration_variable,
        "USUBJID",
        "trta_detector"
      ))
    )
  if (!overall) {
    res <- data_filtered |>
      # Keep only the first occurrence of each AE for each participant
      dplyr::group_by(.data$USUBJID, .data[[variable]]) |>
      # Sort by time until AE occurrence
      dplyr::arrange(.data[[ae_duration_variable]]) |>
      dplyr::slice_head(n = 1) |> # Extract the first occurrence
      dplyr::ungroup() |>
      # Count the number of events and sum of exposure by treatment and variable group
      dplyr::group_by(
        .data[[variable]],
        .data$trta_detector,
        .drop = FALSE
      ) |>
      dplyr::summarise(
        # Number of events
        count = dplyr::n(),
        # total exposure time
        sum_durexp = sum(.data[[exposure_duration_variable]], na.rm = TRUE),
        # time prior to event
        sum_aedur = sum(.data[[ae_duration_variable]], na.rm = TRUE),
        # time censored (after AE)
        sum_censored = .data$sum_durexp - .data$sum_aedur,
        .groups = "drop"
      )
  } else if (overall) {
    res <- data_filtered |>
      # Keep only the first occurrence of *any* AE for each participant
      dplyr::group_by(.data$USUBJID) |>
      # Sort by time until AE occurrence
      dplyr::arrange(.data[[ae_duration_variable]]) |>
      dplyr::slice_head(n = 1) |> # Extract the first occurrence
      dplyr::ungroup() |>
      # Count the number of events and sum of exposure by treatment group
      dplyr::group_by(
        .data$trta_detector,
        .drop = FALSE
      ) |>
      dplyr::summarise(
        # Number of events
        count = dplyr::n(),
        # total exposure time
        sum_durexp = sum(.data[[exposure_duration_variable]], na.rm = TRUE),
        # time prior to event
        sum_aedur = sum(.data[[ae_duration_variable]], na.rm = TRUE),
        # time censored (after AE)
        sum_censored = .data$sum_durexp - .data$sum_aedur,
        .groups = "drop"
      ) |>
      # Add "OVERALL" as variable group
      dplyr::mutate(
        {{ variable }} := "OVERALL"
      )
  }
  res
}

#' Calculate incidence rates
#' @param data Combined ADSL-ADAE data with incidence rate numerators and denominators
#' @inheritParams calculate_results
add_incidence_rates <- function(
  data,
  variable
) {
  # Previous checks
  assert_columns(
    data,
    c(
      variable,
      "big_exp",
      "sum_censored",
      "count",
      "trta_detector",
      "big_n"
    )
  )
  res <- data |>
    dplyr::mutate(
      # time-at-risk is obtained subtracting the time of exposure censured
      # after the AE from the total exposure time
      pattime = .data$big_exp - .data$sum_censored,
      # Avoid division by zero when pattime is 0 or negative (e.g. all exposure censored)
      prop = dplyr::if_else(
        .data$pattime > 0,
        365.25 * 100 * .data$count / .data$pattime,
        NA_real_
      ),
      no_count = .data$big_n - .data$count
    ) |>
    dplyr::select(
      tidyselect::all_of(variable),
      "trta_detector",
      "count",
      "no_count",
      "big_n",
      "pattime",
      "prop"
    )
  res
}

#' Get rates data
#' @inheritParams add_incidence_numerators
#' @inheritParams join_denominators
get_rates_data <- function(
  data,
  variable,
  ae_duration_variable,
  exposure_duration_variable,
  overall = FALSE,
  data_denominators
) {
  res <- data |>
    add_incidence_numerators(
      variable = variable,
      ae_duration_variable = ae_duration_variable,
      exposure_duration_variable = exposure_duration_variable,
      overall = overall
    ) |>
    join_denominators(data_denominators) |>
    add_incidence_rates(variable)
  res
}

#' Calculate counts and proportions (or incidence rates) of events
#' @inheritParams calculate_results
#' @param comb_data Filtered dataset combining ADSL and ADAE
#' @param big_n Tibble with denominators for treatment arms (number of subjects)
#' @param variable A character, the variable name that will be shown in the plot
#'  ("AEDECOD", "MLG_label", "SMQ_NAME", "ocmq", or "AEBODSYS").
#' @param frequency_measure Either "proportions" or "incidence rates"
#' @returns A tibble with columns:
#' \describe{
#'   \item{\code{variable}}{The safety variable, can be "AEDECOD", "MLG_label",
#' "SMQ_NAME", "ocmq" or "AEBODSYS"}
#'   \item{\code{trta_detector}}{Treatment arm, "Verum" or "Comparison"}
#'   \item{\code{count}}{Number of events}
#'   \item{\code{no_count}}{Number of no-events (total minus number of events)}
#'   \item{\code{big_n}}{Number of subjects in each arm}
#'   \item{\code{prop}}{Proportion or incidence rate}
#'   \item{\code{pattime}}{Text exposure time per arm, only if incidence rates}
#' }
get_count_proportions <- function(
  comb_data,
  big_n,
  variable = c("AEDECOD", "MLG_label", "SMQ_NAME", "ocmq", "AEBODSYS"),
  frequency_measure = c("proportions", "incidence rates"),
  duration_mode,
  ae_duration_variable = NULL,
  exposure_duration_variable = NULL,
  exposure_start_variable = NULL,
  exposure_end_variable = NULL,
  ae_start_variable = NULL
) {
  variable <- match.arg(variable)
  frequency_measure <- match.arg(frequency_measure)
  if (frequency_measure == "proportions") {
    # Logic for incidence proportions -----
    proportions_ae <- comb_data |>
      get_proportions_data(
        variable = variable,
        overall = FALSE,
        data_denominators = big_n
      )

    proportions_overall <- comb_data |>
      get_proportions_data(
        variable = variable,
        overall = TRUE,
        data_denominators = big_n
      )

    proportions_data <- dplyr::bind_rows(
      proportions_ae,
      proportions_overall
    )
    assertthat::assert_that(
      "OVERALL" %in% proportions_data[[variable]]
    )

    proportions_data
  } else if (frequency_measure == "incidence rates") {
    # Logic for incidence rates -----
    assertthat::assert_that(duration_mode %in% c("duration", "start_end_date"))
    if (duration_mode == "start_end_date") {
      comb_data <- comb_data |>
        # Derive durations from start and end dates
        add_durations(
          ae_start_variable = ae_start_variable,
          exposure_start_variable = exposure_start_variable,
          exposure_end_variable = exposure_end_variable
        )
      exposure_duration_variable <- "exposure_duration"
      ae_duration_variable <- "ae_duration"
    }

    assert_columns(big_n, c("trta_detector", "big_n", "big_exp"))
    # Calculate times at risk and event count
    rates_ae <- comb_data |>
      get_rates_data(
        variable = variable,
        ae_duration_variable = ae_duration_variable,
        exposure_duration_variable = exposure_duration_variable,
        data_denominators = big_n,
        overall = FALSE
      )

    rates_overall <- comb_data |>
      get_rates_data(
        variable = variable,
        ae_duration_variable = ae_duration_variable,
        exposure_duration_variable = exposure_duration_variable,
        data_denominators = big_n,
        overall = TRUE
      )

    rates_data <- dplyr::bind_rows(
      rates_ae,
      rates_overall
    )
    assertthat::assert_that("OVERALL" %in% rates_data[[variable]])
    rates_data
  }
}
