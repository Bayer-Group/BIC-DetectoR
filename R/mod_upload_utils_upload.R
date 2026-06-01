# Helpers for uploading files ----
#' Upload valid file
#' @param file A file uploaded from shiny::fileInput()
#' @returns A dataframe.
upload_file <- function(file) {
  path <- file$datapath
  ext <- tools::file_ext(file$name)
  switch(
    ext,
    csv = vroom::vroom(path, delim = ","),
    sas7bdat = haven::read_sas(path),
    rds = readRDS(path),
    Rdta = readRDS(path),
    shiny::validate(
      "Invalid file; Please upload a .sas7bdat, .rds, .Rdta, or .csv file"
    )
  )
}

#' Update available variables for variable selection
#' @param data_req Required dataset (usually, ADAE or ADSL).
#' @param var_names Vector with all column names of the corresponding dataset.
#' @param name_pattern A regexp pattern, with the most common names of the
#' variables. E.g., "SAFFL|SAFFN" for the safety population flag variable.
#' @param input_id The ID of the input to update (character).
#' @param session The Shiny session.
#'
update_variables_input <- function(
  data_req,
  var_names,
  name_pattern,
  input_id,
  session
) {
  shiny::req(data_req)
  choices <- reorder_pattern(var_names, name_pattern)
  shiny::updateSelectInput(
    session,
    input_id,
    choices = choices
  )
}

#' Update available values for variable selection
#' @param data_req Required dataset (usually, ADAE or ADSL).
#' @param input_req Required input (the corresponding variable).
#' @param input_id The ID of the input to update (character).
#' @param session The Shiny session.
update_values_input <- function(
  data_req,
  input_req,
  input_id,
  session
) {
  shiny::req(data_req, input_req)
  choices <- unique(data_req[[input_req]]) |>
    # By default, select Y or 1
    reorder_pattern("Y|1")
  shinyWidgets::updatePickerInput(
    session,
    input_id,
    choices = choices,
    selected = choices[1]
  )
}
