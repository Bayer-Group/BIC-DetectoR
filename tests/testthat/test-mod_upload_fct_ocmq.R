test_that("joining with OCMQs works", {
  adae <- adae_data
  adsl <- adsl_data
  adsl_adae <- join_adsl_adae(adsl, adae)

  res <- adsl_adae |> join_ocmq_data()

  expect_s3_class(res, "tbl")
  non_missing_ocmq <- sum(!is.na(res$ocmq))
  expect_gt(non_missing_ocmq, 0)

  # Expect many-to-many matches
  expect_gt(nrow(res), nrow(adsl_adae))

  # All AEs inside OCMQ narrow should be inside OCMQ broad
  aes_narrow <- res |>
    dplyr::filter(scope == "Narrow") |>
    dplyr::pull("AEDECOD")
  aes_broad <- res |>
    dplyr::filter(scope == "Broad") |>
    dplyr::pull("AEDECOD")
  expect_all_true(aes_narrow %in% aes_broad)
})
