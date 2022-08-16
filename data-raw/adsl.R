## code to prepare `adsl` dataset goes here
adsl3 <- readRDS(here::here("data-raw", "adsl_demo_data_22_07_2022.Rdta"))
adsl_data <- adsl3  ## renamed to match original DI_safety code

usethis::use_data(adsl_data, overwrite = TRUE)
adsl <- adsl3  ## renamed to match original DI_safety code

usethis::use_data(adsl, overwrite = TRUE)