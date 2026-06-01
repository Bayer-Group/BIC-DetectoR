#' Check that adverse event dataset has the required variables
#'
#' This function runs after uploading an ADAE file in a valid extension
#' @param adae A dataframe with ADAE data.
#'
check_adae_data <- function(adae) {
  checkmate::assert_data_frame(adae, min.rows = 1)
  # Check for required columns and throw an error otherwise
  req_columns <- c("USUBJID", "STUDYID", "AEDECOD", "AEBODSYS")
  assert_columns(adae, req_columns)

  # Check for AEPTCD, M_PT, pt_code in ADAE
  if (!any(c("AEPTCD", "M_PT", "pt_code") %in% colnames(adae))) {
    rlang::abort(
      message = paste("Missing variable AEPTCD, M_PT or pt_code"),
      class = "adae_missing_aeptcd"
    )
  }
  # Rename common old variable names
  if ("M_PT" %in% colnames(adae) && !"AEPTCD" %in% colnames(adae)) {
    adae <- adae |>
      dplyr::rename(AEPTCD = "M_PT")
  }
  # For older versions change variable name pt_code into AEPTCD
  if ("pt_code" %in% colnames(adae) && !"AEPTCD" %in% colnames(adae)) {
    adae <- adae |>
      dplyr::rename(AEPTCD = "pt_code")
  }
  adae |>
    dplyr::rename_with(toupper) |>
    # To avoid problems when joining (ensure it's character, not double)
    dplyr::mutate(AEPTCD = as.character(.data$AEPTCD))
}

#' Select adverse event data (used in DetectoR app)
#'
#' @param mode A character, "sas" or "demo". Whether to use uploaded files or
#'  demo data.
#' @param adae A dataframe. ADAE dataset output from check_adae_data(). NULL if
#'  using demo data.
#'
#' @return Transformed adverse event dataset.
#'
prepare_adae_data <- function(
  mode = c("demo", "sas"),
  adae = NULL
) {
  mode <- match.arg(mode)

  if (mode == "sas") {
    assert_is_not_null(adae)
    adae <- adae
  } else if (mode == "demo") {
    adae <- adae_data
  }

  adae
}

#' Check that ADSL dataset has the required variables
#'
#' This function runs after uploading an ADSL file in a valid extension
#' @param adsl A dataframe with ADSL data.
#'
check_adsl_data <- function(adsl) {
  checkmate::assert_data_frame(adsl, min.rows = 1)
  # Check for required columns and throw an error otherwise
  req_columns <- c("USUBJID", "STUDYID")
  assert_columns(adsl, req_columns)

  adsl |>
    dplyr::rename_with(toupper)
}

#' Select subject level data (used in DetectoR app)
#'
#' @param mode A character, "sas" or "demo". Whether to use uploaded files or
#'  demo data.
#' @param adsl A dataframe. ADSL dataset output from check_adsl_data(). NULL if
#'  using demo data.
#' @return A filtered subject level data set.
#'
prepare_adsl_data <- function(
  mode = c("demo", "sas"),
  adsl = NULL
) {
  mode <- match.arg(mode)

  if (mode == "sas") {
    assert_is_not_null(adsl)
    adsl <- adsl
  } else if (mode == "demo") {
    adsl <- adsl_data
  }

  checkmate::assert_data_frame(adsl, min.rows = 1)
  adsl
}
