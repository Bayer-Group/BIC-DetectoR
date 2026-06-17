# integrated test calculate_results() ----
test_that("calculate_results creates a valid tibble", {
  data_adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(.data$TREATMGR)
    ) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1
    )
  data_adae <- adae_data
  joint_data <- data_adsl |>
    dplyr::left_join(data_adae, by = c("USUBJID", "STUDYID"))
  variable <- "AEDECOD"
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = data_adsl,
    variable = variable,
    effect_measure = "RR",
    adjustment = "DFDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "proportions"
  )
  number_unique_pts <- data_adae |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::distinct(.data$AEDECOD) |>
    nrow()
  expect_s3_class(results, "tbl") # Result is a tibble
  expect_true(nrow(results) > 0) # Result is not an empty tibble
  expect_equal(nrow(results), number_unique_pts * 2) # Two rows per AEDECOD
  expect_true(sum(!is.na(results$DFDR)) > 0) # Some non-missing DFDR p-values
  expect_true(sum(is.na(results$DFDR)) > 0) # Some missing DFDR p-values
  expect_true(sum(results$DFDR_label == "<0.0001") > 0) # Some DFDR p-values
})

test_that("calculate_results stratified calculation works", {
  variable <- "AEDECOD"
  data_adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(.data$TREATMGR)
    ) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1
    )
  data_adae <- adae_data |>
    # Filtering a subgroup because stratified analysis takes a long time
    dplyr::filter(
      .data$AEBODSYS %in%
        c(
          "Infections and infestations",
          "Gastrointestinal disorders"
        )
    )
  joint_data <- data_adsl |>
    dplyr::left_join(data_adae, by = c("USUBJID", "STUDYID"))
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = data_adsl,
    variable = variable,
    effect_measure = "RR",
    adjustment = "DFDR",
    order_by = "p-value",
    study_strat = "STUDYID",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "proportions"
  )
  number_unique_pts <- joint_data |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::distinct(.data[[variable]]) |>
    nrow()
  expect_s3_class(results, "tbl") # Result is a tibble
  expect_true(nrow(results) > 0) # Result is not an empty tibble
  expect_equal(nrow(results), number_unique_pts * 2) # Two rows per AEDECOD
  expect_true(sum(!is.na(results$DFDR)) > 0) # Some non-missing DFDR p-values
  expect_true(sum(is.na(results$DFDR)) > 0) # Some missing DFDR p-values
  expect_true(sum(results$DFDR_label == "<0.0001") > 0)
  # Some DFDR p-values highly significant
})

# Validate input data sets ----
test_that("input for get_fisher_rr_proportions() has all necessary columns", {
  data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds")) |>
    dplyr::select(-"trta_detector")
  expect_error(
    get_fisher_rr_proportions(
      data,
      alternative = "two.sided",
      alpha = 0.05,
      variable = "AEDECOD"
    ),
    regexp = "Missing variables"
  )
})

test_that("input data for get_fisher_rr_rates() has all necessary columns", {
  data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds")) |>
    dplyr::select(-"trta_detector")
  expect_error(
    get_fisher_rr_rates(
      data,
      alternative = "two.sided",
      alpha = 0.05,
      variable = "AEDECOD"
    ),
    regexp = "Missing variables"
  )
})

# Validate intermediate data created by the function ----
test_that("check intermediate input for get_fisher_rr_proportions()", {
  data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds")) |>
    dplyr::mutate(trta_detector = factor(c("fake_name_a", "fake_name_b")))
  expect_error(
    get_fisher_rr_proportions(
      data,
      alternative = "two.sided",
      alpha = 0.05,
      variable = "AEDECOD"
    ),
    regexp = "Must have levels"
  )
})

testthat::test_that("get_fisher_rr works", {
  data <- data.frame(
    AEDECOD = c("test", "test"),
    trta_detector = factor(c("Verum", "Comparator")),
    count = c(3, 1),
    no_count = c(7, 9),
    prop = c(0.3, 0.1),
    big_n = c(10, 10)
  )
  testthat::expect_no_error(
    get_fisher_rr_proportions(data, variable = "AEDECOD")
  )
})

testthat::test_that("get_fisher_rr fails with missing values", {
  data <- data.frame(
    AEDECOD = c("test", "test"),
    trta_detector = factor(c("Verum", "Comparator")),
    count = c(3, NA),
    no_count = c(7, 9),
    prop = c(0.3, 0.1),
    big_n = c(10, 10)
  )
  testthat::expect_error(
    get_fisher_rr_proportions(data, variable = "AEDECOD"),
    regexp = "missing values"
  )
})

# get_rr() ----
test_that("Zero-value correction works", {
  testthat::expect_no_error(get_rr(0, 100, 10, 100, alpha = 0.05))
  rr <- get_rr(0, 100, 10, 100, alpha = 0.05)
  testthat::expect_equal(sum(rr == 0), 0)
  testthat::expect_equal(sum(is.na(rr)), 0)
})

test_that("Zero-value correction doesn't break when there are no zeroes", {
  testthat::expect_no_error(get_rr(5, 100, 10, 100, alpha = 0.05))
})

test_that("get_rr fails with any NA value", {
  testthat::expect_error(
    get_rr(5, 10, NA, NA, alpha = 0.05),
    regexp = "missing values"
  )
})

# test get_fdr_p_values() ----
test_that("creates a tibble dataframe", {
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  results <- get_fdr_p_values(res_data)
  expect_s3_class(results, "data.frame")
  expect_s3_class(results, "tbl")
})

test_that("creates p_adj column", {
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  results <- get_fdr_p_values(res_data)
  expect_true("p_adj" %in% colnames(results))
})

test_that("p_adj is numeric double", {
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  results <- get_fdr_p_values(res_data)
  expect_vector(results$p_adj, numeric())
  expect_vector(results$p_adj, double())
})

test_that("p_adj has no missing values", {
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  results <- get_fdr_p_values(res_data)
  expect_equal(sum(is.na(results$p_adj)), 0)
})

test_that("custom functions works equal to c212.BH.adjust.pvals", {
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  res_c212 <- c212::c212.BH.adjust.pvals(res_data)
  res_custom <- get_fdr(res_data)
  expect_equal(res_custom$FDR, res_c212$p_adj)
})

# test count_total_proportions() ----
test_that("there are no missing total proportions", {
  ddp_data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds"))
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  res_fdr <- get_fdr_p_values(res_data)
  variable <- "AEDECOD"
  res2 <- ddp_data |>
    dplyr::left_join(res_fdr, by = variable)
  results <- count_total_proportions(res2, "AEDECOD")
  expect_equal(sum(is.na(results$prop)), 0)
})

test_that("there are proportions calculated for each pair of rows", {
  ddp_data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds"))
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  length_dpp_data <- nrow(ddp_data)
  res_fdr <- get_fdr_p_values(res_data)
  variable <- "AEDECOD"
  res2 <- ddp_data |>
    dplyr::left_join(res_fdr, by = variable)
  results <- count_total_proportions(res2, "AEDECOD")
  expect_length(results$prop, length_dpp_data / 2)
})

test_that("there are no repeated rows", {
  comb_data <- readRDS(test_path("fixtures", "comb_data.rds"))
  ddp_data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds"))
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  equivalence_pt_soc <- comb_data |>
    dplyr::distinct(.data$AEDECOD, .data$AEBODSYS)
  res_fdr <- get_fdr_p_values(res_data)
  res2 <- ddp_data |>
    dplyr::left_join(res_fdr, by = "AEDECOD")
  results <- count_total_proportions(res2, "AEDECOD") |>
    dplyr::left_join(equivalence_pt_soc, by = "AEDECOD")
  expect_length(results$prop, length(unique(results$AEDECOD)))
})

# test reorder_levels() ----
test_that("reordering levels works", {
  ddp_data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds"))
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  res_fdr <- get_fdr_p_values(res_data)
  res2 <- ddp_data |>
    dplyr::left_join(res_fdr, by = "AEDECOD")
  results_rr <- reorder_levels(
    res2,
    "AEDECOD",
    order_by = "effect",
    effect_measure = "RR",
    adjustment = "FDR"
  )
  results_vector_rr <- levels(results_rr$axis_var)[
    levels(results_rr$axis_var) != "OVERALL"
  ]
  expected_vector_rr <- res2 |>
    dplyr::arrange(desc(.data$rr), .data$p, .data$AEDECOD) |>
    dplyr::distinct(.data$AEDECOD) |>
    dplyr::pull(.data$AEDECOD) |>
    rev()
  attr(expected_vector_rr, "format.sas") <- attr(
    expected_vector_rr,
    "label"
  ) <- NULL
  results_fdr <- reorder_levels(
    res2,
    "AEDECOD",
    order_by = "p-value",
    effect_measure = "RR",
    adjustment = "FDR"
  )
  results_vector_fdr <- levels(results_fdr$axis_var)[
    levels(results_fdr$axis_var) != "OVERALL"
  ]
  expected_vector_fdr <- res2 |>
    dplyr::arrange(.data$p_adj, .data$p, .data$AEDECOD) |>
    dplyr::distinct(.data$AEDECOD) |>
    dplyr::pull(.data$AEDECOD) |>
    rev()
  attr(expected_vector_fdr, "format.sas") <- attr(
    expected_vector_fdr,
    "label"
  ) <- NULL
  expect_equal(results_vector_rr, expected_vector_rr)
  expect_equal(results_vector_fdr, expected_vector_fdr)
})

# test get_new_dfdr() ----
test_that("one p-value label for each AE", {
  comb_data <- readRDS(test_path("fixtures", "comb_data.rds"))
  ddp_data <- readRDS(test_path("fixtures", "double_dot_plot_data.rds"))
  res_data <- readRDS(test_path("fixtures", "results_data.rds"))
  equivalence_pt_soc <- comb_data |>
    dplyr::distinct(.data$AEDECOD, .data$AEBODSYS)
  res_fdr <- get_fdr_p_values(res_data)
  res2 <- ddp_data |>
    dplyr::left_join(res_fdr, by = "AEDECOD")
  res3 <- count_total_proportions(res2, "AEDECOD") |>
    dplyr::left_join(equivalence_pt_soc, by = "AEDECOD")
  results <- get_new_dfdr(res3, "AEDECOD", 0.05)
  expect_length(results$DFDR, length(unique(comb_data$AEDECOD)))
  expect_equal(sum(is.na(results$DFDR_label)), 0)
})

# test get_stratified_rr_rd() ----
testthat::test_that("All sub-steps for stratified analysis work", {
  # Input -----
  frequency_measure <- "proportions"
  variable <- "AEBODSYS"
  stratify_variable <- "RCEGR01"
  alpha <- 0.05
  filter <- "one_percent"
  effect_measure <- "RD"
  alternative <- "two.sided"
  joint_data <- readRDS(test_path("fixtures", "comb_data.rds")) |>
    dplyr::mutate(trta_detector = factor(.data$TREATMGR.x))
  adsl_filtered_data <- adsl_data |>
    dplyr::mutate(trta_detector = factor(.data$TREATMGR)) |>
    dplyr::filter(.data$SAFFN == 1)
  # End of input ----
  comb_data <- filter_empty_variable(joint_data, variable)
  big_n <- get_big_n(adsl_filtered_data, frequency_measure)
  proportions_data <- comb_data |>
    get_count_proportions(
      big_n = big_n,
      variable = variable,
      frequency_measure = frequency_measure
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
  # Results stratified
  results_stratified <- comb_data |>
    get_stratified_rr_rd(
      variable = variable,
      effect_measure = effect_measure,
      alpha = alpha,
      stratify_variable = stratify_variable
    ) |>
    dplyr::filter(.data[[variable]] %in% filtered_data[[variable]])
  # Results not stratified
  results_full <- filtered_data |>
    get_fisher_rr_proportions(
      alternative = alternative,
      alpha = alpha,
      variable = variable
    ) |>
    dplyr::filter(.data[[variable]] != "OVERALL")
  testthat::expect_equal(nrow(results_stratified), nrow(results_full))
})

# testServer ----
shiny::testServer(
  mod_calculate_server,
  args = list(
    r = list(meddra_mode = "with_meddra"),
    calculate_mode = "double_dot"
  ),
  {
    # Options (5): SOCs, MLGs, SMQs, PTs, OCMQs
    expect_equal(length(meddra_variable_choices()), 5)
  }
)
shiny::testServer(
  mod_calculate_server,
  args = list(
    r = list(meddra_mode = "without_meddra"),
    calculate_mode = "double_dot"
  ),
  {
    # Options (3): SOCs, PTs, OCMQs
    expect_equal(length(meddra_variable_choices()), 3)
  }
)

# test filter minimum ae ----
testthat::test_that("Filter minimum number AEs works", {
  adae <- adae_data
  adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(TREATMGR)
    )
  joint_data <- join_adsl_adae(adsl, adae)
  frequency_measure <- "proportions"
  variable <- "AEBODSYS"
  order_by <- "p-value"
  effect_measure <- "RR"
  adjustment <- "FDR"
  number_aes <- 25
  alpha <- 0.05
  alternative <- "two.sided"
  duration_mode <- "none"
  filter <- "one_percent"

  # Helper to count distinct categories
  get_n_unique <- function(data) {
    length(unique(data[[variable]]))
  }

  get_n_missing <- function(data) {
    sum(is.na(data[[variable]]))
  }

  # Big N
  big_n <- get_big_n(
    adsl_filtered_data = adsl,
    frequency_measure = frequency_measure,
    duration_mode = duration_mode
  )
  # Proportion data
  proportions_data <- joint_data |>
    get_count_proportions(
      big_n = big_n,
      variable = variable,
      frequency_measure = frequency_measure,
      duration_mode = duration_mode
    )
  expect_equal(get_n_missing(proportions_data), 0)
  n_distinct_categories <- get_n_unique(proportions_data)

  # Add filters of minimum count of AEs
  filtered_data <- proportions_data |>
    filter_minimum_aes(
      variable = variable,
      big_n = big_n,
      method = filter,
      alternative = alternative,
      alpha = alpha
    )

  expect_equal(get_n_missing(filtered_data), 0)
  n_distinct_categories_filtered <- get_n_unique(filtered_data)
  expect_lt(n_distinct_categories_filtered, n_distinct_categories)

  # Calculate RR and RD
  effect_data <- filtered_data |>
    get_fisher_rr_proportions(
      alternative = alternative,
      alpha = alpha,
      variable = variable
    )
  expect_equal(get_n_missing(effect_data), 0)
  n_distinct_effect_data <- get_n_unique(effect_data)
  expect_equal(n_distinct_effect_data, n_distinct_categories_filtered)

  # Calculate FDR-adjusted p-values
  effect_fdr_data <- effect_data |>
    get_fdr_p_values() |>
    flag_significant(
      alpha = alpha,
      adjustment = "FDR",
      effect_measure = effect_measure
    )
  expect_equal(get_n_missing(effect_fdr_data), 0)
  n_distinct_fdr <- get_n_unique(effect_fdr_data)
  expect_equal(n_distinct_effect_data, n_distinct_fdr)

  # Join back
  results_fdr_data <- filtered_data |>
    dplyr::left_join(effect_fdr_data, by = variable)
  expect_equal(get_n_missing(results_fdr_data), 0)
  results_fdr_data <- get_n_unique(results_fdr_data)
  expect_equal(results_fdr_data, n_distinct_fdr)

  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = adsl,
    variable = variable,
    filter = "one_percent"
  ) |>
    # Reorder factor levels
    reorder_levels(
      variable = variable,
      order_by = order_by,
      effect_measure = effect_measure,
      adjustment = adjustment
    ) |>
    # Reorder and filter number of categories
    arrange_data(
      order_by = order_by,
      adjustment = adjustment,
      effect_measure = effect_measure,
      number_aes = number_aes
    )
  expect_equal(sum(is.na(results$prob1)), 0)
  expect_equal(sum(is.na(results$prob2)), 0)
  expect_equal(get_n_unique(results), n_distinct_categories_filtered)
})

# test overall results row for double dot plot ----
testthat::test_that("Overall results are calculated", {
  adae <- adae_data
  adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(TREATMGR)
    )
  joint_data <- join_adsl_adae(adsl, adae)
  frequency_measure <- "proportions"
  variable <- "AEBODSYS"
  order_by <- "p-value"
  effect_measure <- "RR"
  adjustment <- "FDR"
  number_aes <- 25
  alpha <- 0.05
  alternative <- "two.sided"
  duration_mode <- "none"
  filter <- "one_percent"

  # Helper to count distinct categories
  get_n_unique <- function(data) {
    length(unique(data[[variable]]))
  }

  get_n_missing <- function(data) {
    sum(is.na(data[[variable]]))
  }

  # Big N
  big_n <- get_big_n(
    adsl_filtered_data = adsl,
    frequency_measure = frequency_measure,
    duration_mode = duration_mode
  )
  # Count events
  event_count <- joint_data |> count_ae_events(variable)
  expect_s3_class(event_count, "tbl")
  expect_equal(get_n_missing(event_count), 0)
  expect_equal(
    get_n_unique(event_count),
    get_n_unique(joint_data |> dplyr::filter(!is.na(.data[[variable]])))
  )

  # Count overall events
  overall_count <- joint_data |> count_ae_events(variable, overall = TRUE)
  expect_s3_class(overall_count, "tbl")
  expect_equal(nrow(overall_count), 2)
  expect_equal(unique(overall_count[[variable]]), "OVERALL")
  expect_equal(get_n_missing(overall_count), 0)
  expect_equal(sum(is.na(overall_count$count)), 0)

  # Proportion data
  proportions_data <- joint_data |>
    get_count_proportions(
      big_n = big_n,
      variable = variable,
      frequency_measure = frequency_measure,
      duration_mode = duration_mode
    )
  expect_s3_class(proportions_data, "tbl")
  expect_true("OVERALL" %in% proportions_data[[variable]])
  expect_equal(sum(is.na(proportions_data$count)), 0)
  expect_equal(sum(is.na(proportions_data$prop)), 0)
  expect_equal(sum(is.na(proportions_data[[variable]])), 0)

  # Add filters of minimum count of AEs
  filtered_data <- proportions_data |>
    filter_minimum_aes(
      variable = variable,
      big_n = big_n,
      method = filter,
      alternative = alternative,
      alpha = alpha
    )

  expect_equal(get_n_missing(filtered_data), 0)
  expect_lt(get_n_unique(filtered_data), get_n_unique(proportions_data))
  expect_true("OVERALL" %in% filtered_data[[variable]])

  # Calculate RR and RD
  effect_data <- filtered_data |>
    get_fisher_rr_proportions(
      alternative = alternative,
      alpha = alpha,
      variable = variable
    )
  expect_equal(get_n_missing(effect_data), 0)
  expect_equal(get_n_unique(effect_data), get_n_unique(filtered_data))
  expect_true("OVERALL" %in% effect_data[[variable]])

  # Calculate FDR-adjusted p-values
  effect_fdr_data <- effect_data |>
    get_fdr_p_values() |>
    flag_significant(
      alpha = alpha,
      adjustment = "FDR",
      effect_measure = effect_measure
    )
  expect_equal(get_n_missing(effect_fdr_data), 0)
  expect_equal(get_n_unique(effect_fdr_data), get_n_unique(effect_data))

  # Join back
  results_fdr_data <- filtered_data |>
    dplyr::left_join(effect_fdr_data, by = variable)
  expect_equal(get_n_missing(results_fdr_data), 0)
  expect_equal(get_n_unique(results_fdr_data), get_n_unique(effect_fdr_data))
  expect_true("OVERALL" %in% results_fdr_data[[variable]])

  # Test whole flow
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = adsl,
    variable = variable,
    filter = "one_percent"
  )
  expect_equal(sum(is.na(results$prob1)), 0)
  expect_equal(sum(is.na(results$prob2)), 0)
  expect_equal(get_n_unique(results), get_n_unique(filtered_data))
  expect_true("OVERALL" %in% results[[variable]])

  # Reorder factor levels
  results_reordered <- results |>
    reorder_levels(
      variable = variable,
      order_by = order_by,
      effect_measure = effect_measure,
      adjustment = adjustment
    )
  expect_equal(rev(levels(results_reordered$axis_var))[1], "OVERALL")

  # Reorder and filter number of categories
  results_arranged <- results_reordered |>
    arrange_data(
      order_by = order_by,
      adjustment = adjustment,
      effect_measure = effect_measure,
      number_aes = number_aes
    )
  # First row should be OVERALL
  expect_equal(results_arranged[[1, variable]], "OVERALL")
})


# Test results with OCMQ data ----
test_that("OCMQ results work", {
  data_adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(.data$TREATMGR)
    ) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1
    )
  data_adae <- adae_data
  joint_data <- data_adsl |>
    dplyr::left_join(data_adae, by = c("USUBJID", "STUDYID")) |>
    join_ocmq_data()
  variable <- "ocmq"
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = data_adsl,
    variable = variable,
    effect_measure = "RR",
    adjustment = "FDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "proportions"
  )
  expect_s3_class(results, "tbl") # Result is a tibble
  expect_true(nrow(results) > 0) # Result is not an empty tibble
  expect_equal(sum(is.na(results$ocmq)), 0) # No empty categories

  # Counts of broad OCMQs should be >= of narrow OCMQs
  ocmqs <- results |>
    dplyr::count(.data$ocmq, wt = .data$count) |>
    dplyr::mutate(
      scope = dplyr::case_when(
        stringr::str_detect(.data$ocmq, "Broad") ~ "broad",
        stringr::str_detect(.data$ocmq, "Narrow") ~ "narrow",
        .default = NA
      ),
      ocmq = stringr::str_remove_all(.data$ocmq, " - .*$")
    ) |>
    dplyr::filter(
      !is.na(.data$scope)
    ) |>
    tidyr::pivot_wider(
      names_from = "scope",
      values_from = "n",
      values_fill = 0
    )

  expect_all_true(ocmqs$broad >= ocmqs$narrow)
})

test_that("OCMQ results work with incidence rates", {
  data_adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(.data$TREATMGR)
    ) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1
    )
  data_adae <- adae_data
  joint_data <- data_adsl |>
    dplyr::left_join(data_adae) |>
    join_ocmq_data()
  variable <- "ocmq"
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = data_adsl,
    variable = variable,
    effect_measure = "RR",
    adjustment = "FDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "incidence rates",
    duration_mode = "start_end_date",
    exposure_start_variable = "RANDDT",
    exposure_end_variable = "EOSDT",
    ae_start_variable = "ASTDT"
  )
  expect_s3_class(results, "tbl") # Result is a tibble
  expect_true(nrow(results) > 0) # Result is not an empty tibble
  expect_equal(sum(is.na(results$ocmq)), 0) # No empty categories

  # Counts of broad OCMQs should be >= of narrow OCMQs
  ocmqs <- results |>
    dplyr::count(.data$ocmq, wt = .data$count) |>
    dplyr::mutate(
      scope = dplyr::case_when(
        stringr::str_detect(.data$ocmq, "Broad") ~ "broad",
        stringr::str_detect(.data$ocmq, "Narrow") ~ "narrow",
        .default = NA
      ),
      ocmq = stringr::str_remove_all(.data$ocmq, " - .*$")
    ) |>
    dplyr::filter(
      !is.na(.data$scope)
    ) |>
    tidyr::pivot_wider(
      names_from = "scope",
      values_from = "n",
      values_fill = 0
    )

  expect_all_true(ocmqs$broad >= ocmqs$narrow)
})


# Test arrange data with incidence rates ----
test_that("Data arrange works with incidence rates", {
  data_adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(.data$TREATMGR)
    ) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1
    )
  data_adae <- adae_data
  joint_data <- data_adsl |>
    dplyr::left_join(data_adae) |>
    join_ocmq_data()
  variable <- "ocmq"
  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = data_adsl,
    variable = variable,
    effect_measure = "RR",
    adjustment = "FDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "incidence rates",
    duration_mode = "start_end_date",
    exposure_start_variable = "RANDDT",
    exposure_end_variable = "EOSDT",
    ae_start_variable = "ASTDT"
  )
  expect_gt(nrow(results), 0)
  expect_gt(sum(!is.na(results[[variable]])), 0)

  results_reordered <- results |>
    reorder_levels(
      variable = variable,
      order_by = "p-value",
      effect_measure = "RR",
      adjustment = "FDR"
    )
  expect_gt(nrow(results_reordered), 0)
  expect_gt(sum(!is.na(results_reordered$axis_var)), 0)
  expect_equal(rev(levels(results_reordered$axis_var))[1], "OVERALL")

  results_arranged <- results_reordered |>
    arrange_data(
      order_by = "p-value",
      adjustment = "FDR",
      effect_measure = "RR",
      number_aes = 25
    )
  expect_gt(nrow(results_arranged), 0)
})
