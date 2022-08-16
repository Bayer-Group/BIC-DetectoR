## code to prepare `adae` dataset goes here
adae3 <- readRDS(here::here("data-raw", "adae_demo_data_22_07_2022.Rdta"))
adae_data <- adae3  ## renamed to match original DI_safety code

usethis::use_data(adae_data, overwrite = TRUE)
