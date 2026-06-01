testthat::test_that("heatmap colors works", {
  adae <- adae_data
  adsl <- adsl_data |>
    dplyr::mutate(
      trta_detector = factor(TREATMGR)
    )
  joint_data <- join_adsl_adae(adsl, adae) |>
    # Select serious AEs to reduce the range of values
    filter_ae_type(
      type = "serious_treatment_emergent",
      treatment_emergent_variable = "TRTEMFL",
      treatment_emergent_value = "Y",
      serious_variable = "AESER",
      serious_value = "Y",
      drug_related_variable = "AEREL",
      drug_related_value = "Y"
    )
  variable <- "AEDECOD"
  heatmap_variable <- "AEDECOD"
  heatmap_color <- "RR"
  alpha <- 0.05
  color_blue <- "#1E90FF"

  results <- calculate_results(
    joint_data = joint_data,
    adsl_filtered_data = adsl,
    variable = variable
  )
  results_filtered <- results |>
    # Select one SOC to reduce the range of values
    dplyr::filter(AEBODSYS == "Infections and infestations") #|>
  # Fiter even more
  # dplyr::filter(AEDECOD %in% c("Sepsis", "Osteomyelitis"))
  prepared_data <- results_filtered |>
    prepare_heatmap_data(
      heatmap_variable = heatmap_variable,
      heatmap_color = heatmap_color,
      adae_data = adae
    )
  heatmap_counts <- prepared_data |>
    count_heatmap_data(
      heatmap_variable = heatmap_variable
    )
  treemap_data <- make_treemap_data(
    data = prepared_data,
    data_counts = heatmap_counts,
    heatmap_variable = heatmap_variable,
    heatmap_color = heatmap_color,
    alpha = alpha
  )
  treemap_hover <- treemap_data |>
    add_heatmap_hover_text(heatmap_color = heatmap_color)

  treemap_colors <- treemap_hover |>
    add_heatmap_colors(
      heatmap_mode = heatmap_color,
      alpha = alpha,
      theme = "light"
    )
  # Check that if RR < 1, then the color is blue
  colors_rr_less_than_one <- treemap_colors |>
    dplyr::filter(.data$rr < 1) |>
    dplyr::distinct(.data$color) |>
    dplyr::pull()
  expect_equal(colors_rr_less_than_one, color_blue)
})
