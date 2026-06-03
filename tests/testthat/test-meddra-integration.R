# Integrated MedDRA + SMQ/MLG path up to calculate_results() ----
# Demo ADSL/ADAE are aligned to fixture PT codes so join_meddra_data() matches
# the same mock dictionaries used in test-mod_upload.R.

adsl_demo_trta <- function() {
  adsl_data |>
    dplyr::mutate(trta_detector = factor(.data$TREATMGR)) |>
    dplyr::filter(
      .data$trta_detector %in% c("Comparator", "Verum"),
      .data$SAFFN == 1L
    )
}

adae_rows_for_mock_pts <- function(mlg_tbl, n_rows = 800L) {
  pt_keys <- mlg_tbl |>
    dplyr::distinct(.data$PT_NAME, .data$AEPTCD) |>
    dplyr::mutate(
      PT_NAME = as.character(.data$PT_NAME),
      AEPTCD = as.character(.data$AEPTCD)
    )
  n_keys <- nrow(pt_keys)
  adae_data |>
    dplyr::slice_head(n = min(n_rows, nrow(adae_data))) |>
    dplyr::mutate(
      row_id = dplyr::row_number(),
      AEDECOD = pt_keys$PT_NAME[(row_id - 1L) %% n_keys + 1L],
      AEPTCD = pt_keys$AEPTCD[(row_id - 1L) %% n_keys + 1L]
    ) |>
    dplyr::select(-row_id)
}

test_that("Data joined with MLG yield varied FDR p-values", {
  meddra_version <- "1.0"
  meddra_hlt <- prepare_meddra_data(
    readRDS(test_path("fixtures", "mock_meddra.rds")),
    meddra_version
  )
  mlg_tbl <- prepare_mlg_data(
    readRDS(test_path("fixtures", "mock_smq.rds")),
    meddra_version
  )

  adsl_trta <- adsl_demo_trta()
  adae_pt <- adae_rows_for_mock_pts(mlg_tbl)

  joint_data <- join_adsl_adae(adsl_trta, adae_pt) |>
    join_meddra_data(mode = "hlt", meddra_data = meddra_hlt) |>
    join_meddra_data(mode = "mlg", meddra_data = mlg_tbl)

  checkmate::expect_names(
    colnames(joint_data),
    must.include = c("MT_HLT", "MT_HLGT", "MLG_label", "SOC_MLG")
  )

  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = adsl_trta,
    variable = "MLG_label",
    effect_measure = "RR",
    adjustment = "FDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "proportions"
  )

  expect_s3_class(results, "tbl")
  expect_true(nrow(results) > 0L)
  expect_true("p_adj" %in% names(results))
  expect_gt(sum(results$p_adj < 1, na.rm = TRUE), 0L)
  expect_false(all(results$p_adj == 1, na.rm = TRUE))
})

test_that("Data joined with SMQ yield varied FDR p-values", {
  meddra_version <- "1.0"
  smq_tbl <- prepare_smq_data(
    readRDS(test_path("fixtures", "mock_smq.rds")),
    meddra_version
  )

  adsl_trta <- adsl_demo_trta()
  adae_pt <- adae_rows_for_mock_pts(smq_tbl)

  joint_data <- join_adsl_adae(adsl_trta, adae_pt) |>
    join_meddra_data(mode = "smq", meddra_data = smq_tbl)

  checkmate::expect_names(colnames(joint_data), must.include = "SMQ_NAME")

  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = adsl_trta,
    variable = "SMQ_NAME",
    effect_measure = "RR",
    adjustment = "FDR",
    order_by = "p-value",
    study_strat = "None",
    alternative = "two.sided",
    alpha = 0.05,
    filter = "no_method",
    frequency_measure = "proportions"
  )

  expect_s3_class(results, "tbl")
  expect_true(nrow(results) > 0L)
  expect_true("p_adj" %in% names(results))
  expect_gt(sum(results$p_adj < 1, na.rm = TRUE), 0L)
  expect_false(all(results$p_adj == 1, na.rm = TRUE))
})
