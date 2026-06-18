#' Join study data with OCMQ table
#'
#' @param adsl_adae_data A dataframe, output from `join_adsl_adae()`.
#
join_ocmq_data <- function(
  adsl_adae_data
) {
  assert_is_not_null(adsl_adae_data)
  assert_is_not_null(ocmq_data) # internal data
  joint_data <- adsl_adae_data |>
    dplyr::left_join(
      ocmq_data,
      by = c("AEDECOD" = "term"),
      relationship = "many-to-many"
    )
  # Expects many-to-many relationship, because the same PT can appear in
  # several OCMQs, and vice versa

  # Check that data has rows
  checkmate::assert_data_frame(joint_data, min.rows = 1)

  joint_data
}
