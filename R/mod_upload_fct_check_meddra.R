# Functions to manage MedDRA data ----

#' Check MedDRA data from uploaded files
#'
#' @param data A dataframe from uploaded files. Must have columns MT_PT,
#'  MT_HLT, MT_HLGT, VERSION (all characters).
#'
#' @return A tibble with necessary MedDRA variables.
#'
check_meddra_data <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)
  # Check that data has the required variables
  req_columns <- c("MT_PT", "MT_HLT", "MT_HLGT", "VERSION")
  assert_columns(data, req_columns)
  data
}

#' Prepare MedDRA data from uploaded files
#'
#' @param data A dataframe from uploaded files.
#' @param meddra_version A character or numeric. The MedDRA version
#'  (e.g, "28.0", 28, "28.1", 28.1).
#'
#' @return A tibble with filtered MedDRA variables.
#'
prepare_meddra_data <- function(
  data,
  meddra_version
) {
  # Check that meddra_version is a character compliant with MedDRA version
  # format (ending in .0 or .1)
  meddra_version_numeric <- as.numeric(meddra_version)
  meddra_version_character <- format(meddra_version_numeric, nsmall = 1)
  checkmate::assert_character(
    meddra_version_character,
    len = 1,
    any.missing = FALSE,
    pattern = "[0-9]+\\.[01]"
  )

  # Filter the selected meddra_version (useful if the uploaded data
  # contains several MedDRA versions)
  data_filtered <- data |>
    dplyr::filter(.data$VERSION == meddra_version_character)

  # Check that there are still valid rows
  checkmate::assert_data_frame(data_filtered, min.rows = 1)

  data_filtered
}

# Functions to manage MedDRA-SMQ data ----
#' Check MedDRA-SMQ data from uploaded files
#'
#' @param data A dataframe from uploaded files.
#'
#' @return A tibble with necessary MedDRA-SMQ variables.
#'
check_smq_data <- function(data) {
  checkmate::assert_data_frame(data, min.rows = 1)
  # Check that data has the required variables
  req_columns <- c(
    "SMQ_NAME",
    "SMQ_TYPE",
    "SMQ_STATUS",
    "PT_CODE",
    "PT_NAME",
    "SMQ_ASS_SOC_NAME",
    "SMQ_ASS_SOC_CODE",
    "PT_SMQ_NAME",
    "SMQ_MEDDRA_VERSION"
  )
  assert_columns(data, req_columns)
  data
}

#' Prepare MedDRA-MLG data from uploaded files
#'
#' @param data A dataframe from uploaded files
#'
#' @param meddra_version A character or numeric. The MedDRA version
#' (e.g, "28.0", 28, "28.1", 28.1)
#'
#' @return A tibble with filtered MedDRA-MLG variables.
#'
prepare_mlg_data <- function(
  data,
  meddra_version
) {
  # Check that meddra_version is a character compliant with MedDRA version
  # format (ending in .0 or .1)
  meddra_version_numeric <- as.numeric(meddra_version)
  meddra_version_character <- format(meddra_version_numeric, nsmall = 1)
  checkmate::assert_character(
    meddra_version_character,
    len = 1,
    any.missing = FALSE,
    pattern = "[0-9]+\\.[01]"
  )
  available_meddra_versions <- unique(data$SMQ_MEDDRA_VERSION)
  validate_need(
    object = meddra_version_character %in% available_meddra_versions,
    label = "MedDRA data and SMQ data don't include the same MedDRA 
        version! Please re-upload a SMQ data file with the same MedDRA version 
        as the MedDRA data."
  )
  # Filter the selected meddra_version (useful if the uploaded data contains
  # several MedDRA versions)
  mlg_filtered <- data |>
    dplyr::filter(
      .data$SMQ_MEDDRA_VERSION == meddra_version_character,
      # Get only MLG that are current
      .data$SMQ_TYPE == "MLG",
      .data$SMQ_STATUS %in% c("RRL", "PRL")
    )

  # Find PT_SMQ_NAME which are different to SMQ_NAME: "child"
  childs <- mlg_filtered |>
    dplyr::filter(.data$SMQ_NAME != .data$PT_SMQ_NAME) |>
    dplyr::distinct(.data$PT_SMQ_NAME) |>
    dplyr::pull() |>
    as.character()

  # Remove "child" from SMQ_NAME
  mlg_without_childs <- mlg_filtered |>
    dplyr::filter(!(.data$SMQ_NAME %in% childs))

  # Prepare data for merging with ADAE
  mlg <- mlg_without_childs |>
    dplyr::select(
      dplyr::all_of(
        c(
          "SMQ_TYPE",
          "SMQ_NAME",
          "PT_CODE",
          "PT_NAME",
          "SMQ_ASS_SOC_NAME",
          "SMQ_ASS_SOC_CODE",
          "SMQ_STATUS",
          "SMQ_MEDDRA_VERSION"
        )
      )
    ) |>
    dplyr::mutate(PT_CODE = as.character(.data$PT_CODE)) |>
    dplyr::rename(AEPTCD = "PT_CODE") |>
    dplyr::select(
      "AEPTCD",
      "PT_NAME",
      "SMQ_NAME",
      "SMQ_ASS_SOC_CODE",
      "SMQ_ASS_SOC_NAME",
      "SMQ_MEDDRA_VERSION"
    )

  # Check that there are still valid rows
  checkmate::assert_data_frame(mlg, min.rows = 1)

  mlg
}
#' Prepare MedDRA-SMQ data from uploaded files
#' @param data A dataframe from uploaded files
#' @param meddra_version A character or numeric. The MedDRA version
#' (e.g, "28.0", 28, "28.1", 28.1)
#' @return A tibble with filtered MedDRA-SMQ variables.
prepare_smq_data <- function(
  data,
  meddra_version
) {
  # Check that meddra_version is a character compliant with MedDRA version
  # format (ending in .0 or .1)
  meddra_version_numeric <- as.numeric(meddra_version)
  meddra_version_character <- format(meddra_version_numeric, nsmall = 1)
  checkmate::assert_character(
    meddra_version_character,
    len = 1,
    any.missing = FALSE,
    pattern = "[0-9]+\\.[01]"
  )
  available_meddra_versions <- unique(data$SMQ_MEDDRA_VERSION)
  validate_need(
    object = meddra_version_character %in% available_meddra_versions,
    label = "MedDRA data and SMQ data don't include the same MedDRA 
        version! Please re-upload a SMQ data file with the same MedDRA 
        version as the MedDRA data."
  )
  # Filter the selected meddra_version (useful if the uploaded data contains
  # several MedDRA versions)
  smq <- data |>
    dplyr::filter(
      .data$SMQ_MEDDRA_VERSION == meddra_version_character,
      .data$SMQ_TYPE == "SMQ"
    ) |>
    # Select distinct combinations, to avoid double-counting
    dplyr::distinct(.data$SMQ_NAME, .data$PT_NAME, .keep_all = TRUE) |>
    # Convert to factor
    dplyr::mutate(
      dplyr::across(dplyr::all_of(c("SMQ_NAME", "PT_NAME")), factor),
      PT_CODE = as.character(.data$PT_CODE)
    ) |>
    dplyr::rename(AEPTCD = "PT_CODE") |>
    dplyr::select("AEPTCD", "PT_NAME", "SMQ_NAME", "SMQ_MEDDRA_VERSION")
  # Check that there are still valid rows
  checkmate::assert_data_frame(smq, min.rows = 1)

  smq
}
