## studies object

if (file.exists(here::here("data-raw", "studies.csv"))) {
  studyfile <- read.csv(here::here("data-raw", "studies.csv"), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  studies <- studyfile$study
} else {
  studyfile <- data.frame(
    study = character(),
    path = character()
  )
  studies <- c("")
}

usethis::use_data(studyfile, overwrite = TRUE)
usethis::use_data(studies, overwrite = TRUE)
