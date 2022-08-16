## code to prepare `meddra_numbers` dataset goes here

if (file.exists(here::here("data-raw", "Meddra"))) {
  
  meddra_numbers <- list.files(here::here("data-raw", "Meddra"))

  usethis::use_data(meddra_numbers, overwrite = TRUE)

} else {
  
  meddra_numbers <- ""

  usethis::use_data(meddra_numbers, overwrite = TRUE)
}

