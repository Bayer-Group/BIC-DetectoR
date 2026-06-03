test_that("MedDRA data has the required variables", {
  data <- readRDS(test_path("fixtures", "mock_meddra.rds"))
  data_without_columns <- data |> dplyr::select(-MT_PT)
  data_without_rows <- data |> dplyr::filter(is.na(MT_PT))
  expect_error(
    check_meddra_data(data_without_columns),
    regexp = "Missing variables: MT_PT"
  )
  expect_error(check_meddra_data(NULL), regexp = "Must be of type 'data.frame'")
  expect_error(
    check_meddra_data(data_without_rows),
    regexp = "Must have at least 1 rows"
  )
  checkmate::expect_data_frame(check_meddra_data(data), min.rows = 1)
  checkmate::expect_names(
    colnames(check_meddra_data(data)),
    must.include = "VERSION"
  )
})

test_that("MedDRA data is well prepared", {
  data <- readRDS(test_path("fixtures", "mock_meddra.rds"))
  expect_error(
    prepare_meddra_data(data, "1.3"),
    regexp = "Must comply to pattern"
  )
  expect_error(prepare_meddra_data(data, NULL), regexp = "Must have length 1")
  expect_error(
    prepare_meddra_data(data, 0.0),
    regexp = "Must have at least 1 rows"
  )
  checkmate::expect_data_frame(prepare_meddra_data(data, 1.0), min.rows = 1)
  checkmate::expect_data_frame(prepare_meddra_data(data, "1.0"), min.rows = 1)
  checkmate::expect_names(
    colnames(prepare_meddra_data(data, "1.0")),
    must.include = "VERSION"
  )
})

test_that("SMQ data has the required variables", {
  data <- readRDS(test_path("fixtures", "mock_smq.rds"))
  data_without_columns <- data |> dplyr::select(-SMQ_TYPE)
  data_without_rows <- data |> dplyr::filter(is.na(SMQ_TYPE))
  expect_error(
    check_smq_data(data_without_columns),
    regexp = "Missing variables: SMQ_TYPE"
  )
  expect_error(check_smq_data(NULL), regexp = "Must be of type 'data.frame'")
  expect_error(
    check_smq_data(data_without_rows),
    regexp = "Must have at least 1 rows"
  )
  checkmate::expect_data_frame(check_smq_data(data), min.rows = 1)
  checkmate::expect_names(
    colnames(check_smq_data(data)),
    must.include = "SMQ_MEDDRA_VERSION"
  )
})

test_that("mlg data is well prepared", {
  data <- readRDS(test_path("fixtures", "mock_smq.rds"))
  expect_error(prepare_mlg_data(data, "1.3"), regexp = "Must comply to pattern")
  expect_error(prepare_mlg_data(data, NULL), regexp = "Must have length 1")
  expect_error(
    prepare_mlg_data(data, 0.0),
    regexp = "MedDRA data and SMQ data don't include the same MedDRA"
  )
  checkmate::expect_data_frame(prepare_mlg_data(data, 1.0), min.rows = 1)
  checkmate::expect_data_frame(prepare_mlg_data(data, "1.0"), min.rows = 1)
  checkmate::expect_names(
    colnames(prepare_mlg_data(data, "1.0")),
    must.include = c("AEPTCD", "PT_NAME", "SMQ_NAME", "SMQ_MEDDRA_VERSION")
  )
})

test_that("smq data is well prepared", {
  data <- readRDS(test_path("fixtures", "mock_smq.rds"))
  expect_error(prepare_smq_data(data, "1.3"), regexp = "Must comply to pattern")
  expect_error(prepare_smq_data(data, NULL), regexp = "Must have length 1")
  expect_error(
    prepare_smq_data(data, 0.0),
    regexp = "MedDRA data and SMQ data don't include the same MedDRA"
  )
  checkmate::expect_data_frame(prepare_smq_data(data, 1.0), min.rows = 1)
  checkmate::expect_data_frame(prepare_smq_data(data, "1.0"), min.rows = 1)
  checkmate::expect_names(
    colnames(prepare_smq_data(data, "1.0")),
    must.include = c("AEPTCD", "PT_NAME", "SMQ_NAME", "SMQ_MEDDRA_VERSION")
  )
})

test_that("ADAE-ADSL joining works", {
  adsl <- adsl_data
  adae <- adae_data
  joined_data <- join_adsl_adae(adsl, adae)
  checkmate::expect_data_frame(joined_data, min.rows = 1)
  checkmate::expect_names(
    colnames(joined_data),
    must.include = c("USUBJID", "STUDYID", "AEDECOD")
  )
  expect_true(joined_data |> dplyr::filter(!is.na(.data$AEDECOD)) |> nrow() > 0)
  # No duplicated columns names caused by join (ending in .x or .y)
  names <- colnames(joined_data)
  duplicated_colnames <- names[stringr::str_detect(names, "\\.x")]
  expect_length(duplicated_colnames, 0)
})

# Check ADAE required variables ----

test_that("throws error if ADAE data doesn't have required variables", {
  adae <- haven::read_sas(test_path(
    "fixtures",
    "adae_missing_columns.sas7bdat"
  ))
  expect_error(
    check_adae_data(adae),
    class = "missing-columns"
  )
})

test_that("throws error if ADAE data doesn't have AEPTCD, M_PT or pt_code", {
  adae <- haven::read_sas(test_path(
    "fixtures",
    "adae_missing_aeptcd.sas7bdat"
  ))
  expect_error(
    check_adae_data(adae),
    class = "adae_missing_aeptcd"
  )
})

# Check ADSL required variables ----
test_that("throws error if ADSL data doesn't have required variables", {
  adsl <- haven::read_sas(test_path(
    "fixtures",
    "adsl_missing_columns.sas7bdat"
  ))
  expect_error(
    check_adsl_data(adsl),
    class = "missing-columns"
  )
})

# Check normal ADAE and ADSL data ----
test_that("creates normal ADSL data from demo data", {
  adsl <- prepare_adsl_data(mode = "demo")
  expect_s3_class(adsl, "tbl")
  expect_true(nrow(adsl) > 0)
})

test_that("creates normal ADAE data from demo data", {
  adae <- prepare_adae_data(mode = "demo")
  expect_s3_class(adae, "tbl")
  expect_true(nrow(adae) > 0)
})

test_that("creates normal ADAE data from SAS demo data", {
  adae <- haven::read_sas(test_path("fixtures", "adae_demo.sas7bdat"))
  adae <- prepare_adae_data(
    mode = "sas",
    adae = adae
  )
  expect_s3_class(adae, "tbl")
  expect_true(nrow(adae) > 0)
})

test_that("creates normal ADSL data from SAS demo data", {
  adsl <- haven::read_sas(test_path("fixtures", "adsl_demo.sas7bdat"))
  adsl <- prepare_adsl_data(
    mode = "sas",
    adsl = adsl
  )
  expect_s3_class(adsl, "tbl")
  expect_true(nrow(adsl) > 0)
})
