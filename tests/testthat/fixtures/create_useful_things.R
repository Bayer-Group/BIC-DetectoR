# Create demo data in SAS format for testing uploading functions ----
path <- here::here("tests", "testthat", "fixtures")

# Create mock MedDRA dataset for testing ----
pts <- paste0("PT_", 1:30)
hlts <- rep(paste0("HLT_", 1:10), each = 3)
hlgts <- rep(paste0("HLGT_", 1:5), each = 6)
meddras <- tibble::tibble(VERSION = c("1.0", "1.1", "2.0", "2.1"))

mock_meddra <- tibble::tibble(MT_PT = pts, MT_HLT = hlts, MT_HLGT = hlgts) |>
  dplyr::cross_join(meddras)

saveRDS(mock_meddra, file = here::here(path, "mock_meddra.rds"))

# Create mock SMQ dataset for testing ----
mock_smq <- tibble::tibble(
  SMQ_NAME = c(paste0("SMQ_", 1:15), paste0("MLG_", 1:15)),
  SMQ_TYPE = rep(c("SMQ", "MLG"), each = 15),
  SMQ_STATUS = "PRL",
  PT_CODE = as.character(10000001:10000030),
  PT_NAME = paste0("PT_", 1:30),
  PT_SMQ_NAME = c(paste0("SMQ_", 1:15), paste0("MLG_", 1:15)),
  SMQ_ASS_SOC_NAME = rep(paste0("SOC_", 1:5), each = 6),
  SMQ_ASS_SOC_CODE = rep(1:5, each = 6)
) |>
  dplyr::cross_join(meddras) |>
  dplyr::rename("SMQ_MEDDRA_VERSION" = "VERSION")

saveRDS(mock_smq, file = here::here(path, "mock_smq.rds"))

# Mock demo data without required column names ----
adae_missing_columns <- adae_data |> dplyr::select(!c(USUBJID, AEBODSYS))
adae_missing_aeptcd <- adae_data |> dplyr::select(!AEPTCD)
adsl_missing_columns <- adsl_data |> dplyr::select(!c(STUDYID, SAFFN))
saveRDS(adae_missing_columns, here::here(path, "adae_missing_columns.rds"))
saveRDS(adsl_missing_columns, here::here(path, "adsl_missing_columns.rds"))

# Demo data in SAS ----
haven::write_sas(adae_data, here::here(path, "adae_demo.sas7bdat"))
haven::write_sas(adsl_data, here::here(path, "adsl_demo.sas7bdat"))
haven::write_sas(
  adae_missing_columns,
  here::here(path, "adae_missing_columns.sas7bdat")
)
haven::write_sas(
  adae_missing_aeptcd,
  here::here(path, "adae_missing_aeptcd.sas7bdat")
)
haven::write_sas(
  adsl_missing_columns,
  here::here(path, "adsl_missing_columns.sas7bdat")
)
