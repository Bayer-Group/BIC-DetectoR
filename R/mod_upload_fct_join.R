#' Join ADAE and ADSL datasets
#'
#' @param adsl A dataframe, output from `adsl_data_trta()`.
#' @param adae A dataframe, output from `select_adae_data()`.
#'
#' @returns adsl_adae_data, a dataframe of joined ADAE-ADSL data.
#'
join_adsl_adae <- function(
  adsl,
  adae
) {
  # ADSL and ADAE may share variables other than USUBJID and STUDYID
  # For example, TREATMGR in demo data (treatment arm)
  # Joining only by USUBJID and STUDYID results in duplication of variables
  # For example, TREATMGR.x and TREATMGR.y
  common_variables <- colnames(adae)[colnames(adae) %in% colnames(adsl)]
  common_variables <- common_variables[
    !common_variables %in% c("USUBJID", "STUDYID")
  ]
  adae <- adae |>
    dplyr::select(-tidyselect::any_of(common_variables))
  adsl_adae_data <- dplyr::left_join(adsl, adae, by = c("USUBJID", "STUDYID"))
  adsl_adae_data
}

#' Join study data with MedDRA dictionary
#'
#' @param adsl_adae_data A dataframe, output from `join_adsl_adae()`.
#' @param mode A character "hlt" (SOC/HLGTs/HLTs/PTs), "mlg" (SOC/MLGs/PTs) or
#'  "smq" (SMQs).
#' @param meddra_data MedDRA data from `prepare_meddra_data()`.
#'
#' @return Transformed adverse event dataset.
#'
join_meddra_data <- function(
  adsl_adae_data,
  mode = c("hlt", "mlg", "smq"),
  meddra_data
) {
  mode <- match.arg(mode)
  assert_is_not_null(adsl_adae_data)
  assert_is_not_null(meddra_data)
  if (mode == "hlt") {
    # MedDRA hierarchy SOC/HLGT/HLT/PT ----
    joint_data_tmp <- adsl_adae_data |>
      # To avoid problems when joining (ensure it's character, not double)
      dplyr::mutate(AEPTCD = as.character(.data$AEPTCD)) |>
      dplyr::left_join(meddra_data, by = c("AEDECOD" = "MT_PT"))
    # Detect which HLGT are contained in more than one SOC, example:
    # Cholesteatoma and Ear pain are both Ear disorders NEC (HLT) and Aural
    # disorders NEC (HLGT), but
    # first is Neoplasms... (SOC) and second is Ear and labyrinth... (SOC)
    # Create separated HLGT with a number in parenthesis
    # Example:
    # MT_HLGT "Aural disorders NEC" linked to Neoplasms... (SOC)
    # MT_HLGT "Aural disorders NEC (2)" linked to Ear and labyrinth... (SOC)
    hlgt_distinct <- joint_data_tmp |>
      dplyr::select("AEBODSYS", "MT_HLGT", "MT_HLT") |>
      dplyr::distinct(.data$AEBODSYS, .data$MT_HLGT, .keep_all = TRUE) |>
      tidyr::drop_na() |>
      dplyr::group_by(.data$MT_HLGT) |>
      dplyr::mutate(
        hlgt_number = dplyr::case_when(
          dplyr::row_number() == 1 ~ .data$MT_HLGT,
          dplyr::row_number() > 1 ~
            paste0(.data$MT_HLGT, " (", dplyr::row_number(), ")")
        )
      ) |>
      dplyr::ungroup()
    # Same can happen with HLT
    hlt_distinct <- hlgt_distinct |>
      dplyr::distinct(.data$hlgt_number, .data$MT_HLT) |>
      dplyr::group_by(.data$MT_HLT) |>
      dplyr::mutate(
        hlt_number = dplyr::case_when(
          dplyr::row_number() == 1 ~ .data$MT_HLT,
          dplyr::row_number() > 1 ~
            paste0(.data$MT_HLT, " (", dplyr::row_number(), ")")
        )
      ) |>
      dplyr::ungroup()
    # Join all back together
    joint_data <- joint_data_tmp |>
      dplyr::left_join(
        hlgt_distinct |> dplyr::select("AEBODSYS", "MT_HLGT", "hlgt_number"),
        by = c("AEBODSYS", "MT_HLGT")
      ) |>
      dplyr::left_join(hlt_distinct, by = c("hlgt_number", "MT_HLT")) |>
      dplyr::select(-c("MT_HLGT", "MT_HLT")) |>
      dplyr::rename(
        MT_HLGT = "hlgt_number",
        MT_HLT = "hlt_number"
      )
  } else if (mode == "mlg") {
    # MedDRA MLGs ----
    joint_data <- adsl_adae_data |>
      dplyr::mutate(AEPTCD = as.character(.data$AEPTCD)) |>
      dplyr::left_join(meddra_data, by = "AEPTCD") |>
      dplyr::mutate(
        # Create MLG: if no MLG exists for this PT, add the name of the PT
        MLG = ifelse(is.na(.data$SMQ_NAME), .data$AEDECOD, .data$SMQ_NAME),
        # Identify real MLG with (MLG), PTs with (PT)
        MLG_label = dplyr::case_when(
          # observations of subjects from ADSL without AEs
          is.na(.data$AEDECOD) ~ NA,
          is.na(.data$SMQ_NAME) ~ paste0(.data$AEDECOD, " (PT)"),
          !is.na(.data$SMQ_NAME) ~ paste0(.data$SMQ_NAME, " (MLG)")
        ),
        # Create the associated SOC: MLG-associated-SOC for MLGs,
        # or normal SOC for PTs
        SOC_MLG = ifelse(
          is.na(.data$SMQ_ASS_SOC_NAME),
          .data$AEBODSYS,
          .data$SMQ_ASS_SOC_NAME
        )
      )
  } else if (mode == "smq") {
    # MedDRA SMQs ----
    joint_data <- adsl_adae_data |>
      dplyr::mutate(AEPTCD = as.character(.data$AEPTCD)) |>
      dplyr::left_join(
        meddra_data,
        by = c("AEDECOD" = "PT_NAME"),
        relationship = "many-to-many"
      )
    # Expects many-to-many relationship, because the same PT can appear in
    # several SMQs, and vice versa
  }

  # Check that data has rows
  checkmate::assert_data_frame(joint_data, min.rows = 1)

  joint_data
}
