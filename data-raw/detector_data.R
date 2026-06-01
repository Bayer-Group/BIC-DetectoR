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

# Palette of 23 shades of color
# By default, grayscale, then it will be changed using CSS depending on
# light/dark theme

usethis::use_data(
  adae_data,
  adsl_data,
  overwrite = TRUE,
  internal = TRUE
)
