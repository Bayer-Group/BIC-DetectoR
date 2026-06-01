# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ----
## Add meta data about your application and set some default {golem} options
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
golem::fill_desc(
  pkg_name = "DetectoR",
  pkg_title = "Stay Safe; Don't Miss Any Adverse-Event Signal",
  pkg_description = "The DetectoR R Shiny app allows a rapid, rigorous exploration of safety data to uncover hidden adverse‑event signals.",
  authors = c(
    person(
      given = "Carlos",
      family = "Fernandez-Escobar",
      email = "carlos.fernandez4.ext@bayer.com",
      role = c("aut", "cre")
    ),
    person(
      given = "Steffen",
      family = "Jeske",
      role = "aut"
    ),
    person(
      given = "Martin",
      family = "Gebel",
      role = "aut"
    ),
    person(
      given = "Ann-Kathrin",
      family = "Frenz",
      role = "aut"
    )
  ),
  repo_url = "https://github.com/bayer-group/DetectoR",
  pkg_version = "3.0.0",
  set_options = TRUE
)

## Install the required dev dependencies ----
golem::install_dev_deps()

## Create Common Files ----
usethis::use_gpl3_license()
golem::use_readme_rmd(open = FALSE)
devtools::build_readme()

## Init Testing Infrastructure ----
## Create a template for tests
golem::use_recommended_tests()

## Favicon ----
# If you want to change the favicon (default is golem's one)
golem::use_favicon(
  path = "inst/app/www/icons/AppIcon_BAG_DetectoR_210x210mm_RGB.ico"
) # path = "path/to/ico". Can be an online file.
#golem::remove_favicon() # Uncomment to remove the default favicon

## Add helper functions ----
golem::use_utils_ui(with_test = TRUE)
golem::use_utils_server(with_test = TRUE)

## Use git ----
# usethis::use_git()
# ## Sets the remote associated with 'name' to 'url'
# usethis::use_git_remote(
#   name = "origin",
#   url = "https://github.com/<OWNER>/<REPO>.git"
# )

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile("dev/02_dev.R")
