# Edge-case tests for safety and data-handling ----
# Covers: find_min_event, filter_minimum_aes, get_fdr, get_new_dfdr, get_count_proportions

# find_min_event ----
test_that("find_min_event returns NA when no event count yields significant p-value", {
  # Very small sample: only a few event counts are tried; use strict alpha
  # so that none are significant (e.g. n1=10, n2=10, alpha=0.001).
  out <- DetectoR:::find_min_event(
    n1 = 10,
    n2 = 10,
    alternative = "two.sided",
    alpha = 0.001
  )
  expect_identical(out, NA_integer_)
})

test_that("find_min_event returns a finite integer when some count is significant", {
  # Larger n and moderate alpha: at least one event count should be significant
  out <- DetectoR:::find_min_event(
    n1 = 100,
    n2 = 100,
    alternative = "two.sided",
    alpha = 0.05
  )
  expect_true(is.integer(out))
  expect_true(length(out) == 1L)
  expect_true(is.finite(out))
  expect_true(out >= 1L)
})

# filter_minimum_aes ----
test_that("filter_minimum_aes with method 'min' and non-finite min_events returns data unchanged", {
  big_n <- tibble::tibble(
    trta_detector = factor(c("Verum", "Comparator")),
    big_n = c(10L, 10L)
  )
  data <- tibble::tibble(
    AEDECOD = rep(c("AE1", "AE2"), each = 2),
    trta_detector = factor(rep(c("Verum", "Comparator"), 2)),
    count = c(1L, 0L, 2L, 1L),
    big_n = c(10L, 10L, 10L, 10L)
  )
  # When find_min_event returns NA, filter should not drop all rows
  out <- DetectoR:::filter_minimum_aes(
    data = data,
    variable = "AEDECOD",
    big_n = big_n,
    method = "min",
    alternative = "two.sided",
    alpha = 0.001 # strict so min_events is NA
  )
  expect_equal(nrow(out), nrow(data))
  expect_equal(out, data)
})

test_that("filter_minimum_aes one_percent with zero sum_big_n does not error", {
  # One AE group has sum_big_n = 0 (edge case): total_prop becomes NA, row dropped by filter
  data <- tibble::tibble(
    AEDECOD = rep(c("AE1", "AE2"), each = 2),
    trta_detector = factor(rep(c("Verum", "Comparator"), 2)),
    count = c(0L, 0L, 5L, 3L),
    big_n = c(0L, 0L, 10L, 10L) # AE1 has 0 subjects in both arms
  )
  out <- DetectoR:::filter_minimum_aes(
    data = data,
    variable = "AEDECOD",
    big_n = tibble::tibble(
      trta_detector = factor(c("Verum", "Comparator")),
      big_n = c(10L, 10L)
    ),
    method = "one_percent",
    alternative = "two.sided",
    alpha = 0.05
  )
  # AE1 rows have NA total_prop and are filtered out; AE2 remains if >= 1%
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) <= nrow(data))
  expect_false(any(is.infinite(out$count), na.rm = TRUE))
})

test_that("filter_minimum_aes no_method returns data unchanged", {
  data <- tibble::tibble(
    AEDECOD = "AE1",
    trta_detector = factor("Verum"),
    count = 1L,
    big_n = 10L
  )
  big_n <- tibble::tibble(trta_detector = factor("Verum"), big_n = 10L)
  out <- DetectoR:::filter_minimum_aes(
    data = data,
    variable = "AEDECOD",
    big_n = big_n,
    method = "no_method",
    alternative = "two.sided",
    alpha = 0.05
  )
  expect_identical(out, data)
})

# get_fdr ----
test_that("get_fdr with NULL returns NULL", {
  expect_null(DetectoR:::get_fdr(NULL))
})

test_that("get_fdr with 0-row data returns 0-row data with FDR column", {
  data <- tibble::tibble(p = numeric(), x = character())
  out <- DetectoR:::get_fdr(data)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
  expect_true("FDR" %in% colnames(out))
})

test_that("get_fdr with data missing 'p' column returns NULL", {
  data <- tibble::tibble(x = 1:3, y = runif(3))
  expect_null(DetectoR:::get_fdr(data))
})

test_that("get_fdr with single row returns one FDR value", {
  data <- tibble::tibble(p = 0.01)
  out <- DetectoR:::get_fdr(data)
  expect_equal(nrow(out), 1L)
  expect_equal(out$FDR, 0.01)
})

# get_new_dfdr ----
test_that("get_new_dfdr handles SOC with empty or NULL get_fdr result", {
  data <- tibble::tibble(
    AEDECOD = c("PT1", "PT2"),
    AEBODSYS = c("SOC1", "SOC1"),
    count = c(10L, 5L),
    big_n = c(100L, 100L),
    p = c(0.01, 0.03)
  )
  out <- DetectoR:::get_new_dfdr(data, variable = "AEDECOD", alpha = 0.05)
  expect_s3_class(out, "data.frame")
  expect_true("DFDR" %in% colnames(out))
  expect_true("DFDR_label" %in% colnames(out))
  expect_equal(nrow(out), 2L)
})

# get_count_proportions: zero pattime yields NA prop (no division by zero) ----
test_that("get_count_proportions incidence rates with zero pattime gives NA prop not Inf", {
  big_n <- tibble::tibble(
    trta_detector = factor(c("Verum", "Comparator")),
    big_n = c(2L, 2L),
    big_exp = c(0, 200)
  )
  comb_data <- tibble::tibble(
    AEDECOD = c("AE1", "AE1", "AE1", "AE1"),
    USUBJID = c("V1", "V2", "C1", "C2"),
    trta_detector = factor(c("Verum", "Verum", "Comparator", "Comparator")),
    exposure_dur = c(0, 0, 100, 100),
    ae_dur = c(0, 0, 50, 10)
  )
  out <- DetectoR:::get_count_proportions(
    comb_data = comb_data,
    big_n = big_n,
    variable = "AEDECOD",
    frequency_measure = "incidence rates",
    duration_mode = "duration",
    ae_duration_variable = "ae_dur",
    exposure_duration_variable = "exposure_dur"
  )
  expect_false(any(is.infinite(out$prop)))
  expect_true(any(is.na(out$prop)))
  expect_equal(sum(is.na(out$prop)), 1L)
})

# format_p_values with NA ----
test_that("format_p_values returns empty string for NA", {
  expect_equal(DetectoR:::format_p_values(NA_real_), "")
})

test_that("format_p_values returns '<0.0001' for very small p", {
  expect_equal(DetectoR:::format_p_values(1e-10), "<0.0001")
})
