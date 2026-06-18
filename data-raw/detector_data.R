## code to prepare `adsl` dataset goes here
set.seed(2026)
adsl_data <- readRDS(here::here("data-raw", "adsl_data_demo_04_09_2024.rds")) |>
  # Add start and end dates
  dplyr::mutate(
    # randomization date
    RANDDT = as.Date(sample(x = 1:100, size = dplyr::n(), replace = TRUE)),
    EOSDT = as.Date(.data$RANDDT + .data$DUREXP - 1) # end of study date
  )
## code to prepare `adae` dataset goes here
adae_data <- readRDS(here::here("data-raw", "adae_data_demo_04_09_2024.rds")) |>
  # Add start date for AE
  dplyr::left_join(
    adsl_data |> dplyr::select("USUBJID", "RANDDT"),
    by = "USUBJID"
  ) |>
  dplyr::mutate(
    ASTDT = as.Date(.data$RANDDT + .data$AAESDURN - 1) # AE start date
  )

# Save OCMQ data ----
ocmq <- readxl::read_excel(
  here::here("data-raw", "ocmqs_v4.1.xlsx"),
  sheet = "Consolidated List",
  col_types = c("text", "text", "skip", "text"),
  col_names = c("ocmq", "term", "scope"),
  skip = 1
)
# Duplicate Narrow terms to be also counted as Broad Term
ocmq_narrow <- ocmq |>
  dplyr::filter(.data$scope == "Narrow")

ocmq_broad <- ocmq |>
  dplyr::filter(.data$scope == "Broad")

ocmq_data <- dplyr::bind_rows(
  ocmq_narrow,
  ocmq_narrow |> dplyr::mutate(scope = "Broad"),
  ocmq_broad
) |>
  dplyr::mutate(
    ocmq = paste0(.data$ocmq, " - ", scope),
    version = "4.1",
  )

# Add internal data ----
usethis::use_data(
  adae_data,
  adsl_data,
  ocmq_data,
  overwrite = TRUE,
  internal = TRUE
)
