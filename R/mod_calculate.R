#' calculate UI Function
#'
#' @description A shiny Module.
#'
#' @inheritParams mod_upload_ui
#' @param calculate_mode Either "double_dot", "heatmap", "volcano" or "table".
#' Conditionally shows relevant parameters.
#' @noRd
mod_calculate_ui <- function(
  id,
  calculate_mode = c("double_dot", "heatmap", "volcano", "table")
) {
  ns <- shiny::NS(id)
  calculate_mode <- match.arg(calculate_mode)
  bslib::accordion_panel(
    "Display Options",
    icon = shiny::icon("cog"),
    # shiny::verbatimTextOutput(ns("debug")), # Uncomment to see debug prints
    # Standard Panel ----
    ## Action button ----
    bslib::input_task_button(
      ns("go_calculate"),
      "Calculate!",
      icon = shiny::icon("redo"),
      type = "secondary",
      class = "btn-lg"
    ),
    ## Variable parameters ----
    if (calculate_mode %in% c("double_dot", "volcano", "table")) {
      shiny::selectInput(
        ns("safety_variable"),
        "Safety Variable",
        choices = NULL
      )
    } else if (calculate_mode == "heatmap") {
      shiny::selectInput(
        ns("heatmap_variable"),
        "Hierarchy",
        choices = NULL
      )
    },
    ## AE grouping filter ----
    shinyWidgets::pickerInput(
      ns("ae_grouping_filter"),
      "AE Grouping",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = picker_input_options()
    ),
    if (calculate_mode == "double_dot") {
      shiny::radioButtons(
        ns("include_overall"),
        "Include Overall",
        choices = c("Yes" = TRUE, "No" = FALSE),
        selected = FALSE
      )
    },
    ## AE type filter ----
    shiny::selectInput(
      ns("ae_type_filter"),
      "AE Type",
      choices = c(
        "All Treatment-Emergent (TEAEs)" = "treatment_emergent",
        "Treatment-Emergent Serious (TESAEs)" = "serious_treatment_emergent",
        "Drug-Related Treatment-Emergent (TEAEs)" = "drug_related_treatment_emergent",
        "Drug-Related Treatment-Emergent Serious (TESAEs)" = "serious_drug_related_treatment_emergent",
        "All AEs" = "all",
        "All Serious (SAEs)" = "serious",
        "All Drug-Related" = "drug_related",
        "All Drug-Related Serious (SAEs)" = "serious_drug_related"
      )
    ),
    ## Frequency (proportions/incidence rates) ----
    shiny::selectInput(
      ns("frequency_measure"),
      "Frequency Measure",
      choices = NULL
    ),
    ## Effect measure (RR/RD) ----
    if (calculate_mode %in% c("double_dot", "volcano")) {
      shiny::selectInput(
        ns("effect_measure"),
        "Effect Measure",
        choices = list(
          "Relative Risk" = "RR",
          "Risk Difference" = "RD"
        )
      )
    },
    if (calculate_mode == "double_dot") {
      shiny::selectInput(
        ns("order_by"),
        "Order by",
        choices = list(
          "p-value" = "p-value",
          "Effect" = "effect"
        )
      )
    },
    if (calculate_mode == "heatmap") {
      shiny::selectInput(
        ns("heatmap_color"),
        "Colored by",
        choices = list(
          "Relative Risk" = "RR",
          "Risk Difference" = "RD",
          "p-value: False Discovery Rate" = "FDR",
          "p-value: New Double False Discovery Rate" = "DFDR"
        )
      )
    },
    ## Show Advanced Settings ----
    shinyWidgets::materialSwitch(
      ns("switch_advanced_settings"),
      "Show More Settings",
      status = "success",
      value = FALSE
    ),
    # Advanced Settings panel ----
    shiny::conditionalPanel(
      condition = "input.switch_advanced_settings",
      ns = ns,
      if (calculate_mode %in% c("double_dot", "table")) {
        shiny::selectInput(
          ns("adjustment_method"),
          "p-value Adjustment Method",
          choices = NULL
        )
      },
      ## Advanced settings ----
      shiny::selectInput(
        ns("stratified_by"),
        "Stratify by",
        choices = NULL
      ),
      if (calculate_mode == "double_dot") {
        shiny::selectInput(
          ns("number_aes_shown"),
          "Number of AEs Shown (Max)",
          choices = c("25", "50", "100", "1000"),
          selected = "25"
        )
      },
      shiny::selectInput(
        ns("test_alternative"),
        "Alternative for Fisher's Test",
        choices = NULL
      ),
      shiny::selectInput(
        ns("alpha"),
        "Alpha",
        choices = c("0.01", "0.05", "0.1"),
        selected = "0.05"
      ),
      shiny::selectInput(
        ns("aes_filter"),
        "Filter Method",
        choices = NULL
      ),
      ## Collapse axis labels ----
      if (calculate_mode == "double_dot") {
        shinyWidgets::materialSwitch(
          ns("show_full_labels"),
          "Long Labels",
          status = "success",
          value = FALSE
        )
      }
    )
  )
}

#' calculate Server Functions
#' @inheritParams mod_upload_server
#' @noRd
mod_calculate_server <- function(id, r, calculate_mode) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update main parameters ----
    if (calculate_mode %in% c("double_dot", "volcano", "table")) {
      # Parameters for double dot plot ----
      ## Select MedDRA grouping choices ----
      meddra_variable_choices <- shiny::reactive({
        shiny::req(r$meddra_mode)
        meddra_mode <- r$meddra_mode
        if (meddra_mode == "with_meddra") {
          list(
            "System Organ Classes (SOCs)" = "AEBODSYS",
            "Preferred Terms (PTs)" = "AEDECOD",
            "Medical Labeling Groupings (MLGs)" = "MLG_label",
            "Standardised MedDRA Queries (SMQs)" = "SMQ_NAME",
            "Office of New Drugs Custom Medical Queries (OCMQs)" = "ocmq"
          )
        } else if (meddra_mode == "without_meddra") {
          list(
            "System Organ Classes (SOCs)" = "AEBODSYS",
            "Preferred Terms (PTs)" = "AEDECOD",
            "Office of New Drugs Custom Medical Queries (OCMQs)" = "ocmq"
          )
        }
      }) |>
        shiny::bindEvent(r$meddra_mode)
      ## Select adjustment method (FDR, DFDR) ----
      adjustment_choices <- shiny::reactive({
        shiny::req(input$safety_variable)
        variable <- input$safety_variable
        if (variable %in% c("AEDECOD", "MLG_label")) {
          list(
            "False Discovery Rate (FDR)" = "FDR",
            "New Double False Discovery Rate (DFDR)" = "DFDR"
          )
        } else if (variable %in% c("AEBODSYS", "SMQ_NAME", "ocmq")) {
          # SOCs and SMQs don't work with DFDR
          list(
            "False Discovery Rate (FDR)" = "FDR"
          )
        }
      })
      ## Update MedDRA variable choices ----
      shiny::observe({
        shiny::req(meddra_variable_choices())
        shiny::updateSelectInput(
          session,
          "safety_variable",
          choices = meddra_variable_choices()
        )
      }) |>
        shiny::bindEvent(meddra_variable_choices())
      ## Update adjustment method choices ----
      shiny::observeEvent(adjustment_choices(), {
        shiny::req(adjustment_choices())
        shiny::updateSelectInput(
          session,
          "adjustment_method",
          choices = adjustment_choices()
        )
      })
    } else if (calculate_mode == "heatmap") {
      # Heatmap parameters ----
      ## Heatmap hierarchy ----
      heatmap_choices <- shiny::reactive({
        shiny::req(r$meddra_mode)
        meddra_mode <- r$meddra_mode
        if (meddra_mode == "without_meddra") {
          list(
            "SOCs/PTs" = "AEDECOD"
          )
        } else if (meddra_mode == "with_meddra") {
          list(
            "SOCs/PTs" = "AEDECOD",
            "SOCs/HLGTs/HLTs/PTs" = "AEDECOD_HLT",
            "SOCs/MLGs/PTs" = "MLG_label"
          )
        }
      })
      shiny::observeEvent(heatmap_choices(), {
        shiny::req(heatmap_choices())
        shiny::updateSelectInput(
          session,
          "heatmap_variable",
          choices = heatmap_choices(),
          selected = "AEDECOD"
        )
      })
    }
    ## Frequency measure (proportions or incidence rates)----
    shiny::observe({
      shiny::req(r$filtered_data, r$duration_mode)
      if (r$duration_mode != "none") {
        choices <- list(
          "Proportions" = "proportions",
          "Incidence rates" = "incidence rates"
        )
      } else {
        choices <- list("Proportions" = "proportions")
      }
      shiny::updateSelectInput(
        session,
        "frequency_measure",
        choices = choices,
        selected = "proportions"
      )
    }) |>
      shiny::bindEvent(r$filtered_data, r$duration_mode)
    # Advanced settings variables ----

    ## Stratify by ----
    shiny::observeEvent(r$adsl_data, {
      shiny::req(r$adsl_data)
      adsl_variables <- get_variable_labels(r$adsl_data)
      unsuitable_variables <- c(
        "USUBJID",
        "trta_detector",
        "SAFFN",
        "SAFFL",
        "TREATMGR"
      )
      choices <- as.character(c(
        "None",
        adsl_variables[!adsl_variables %in% unsuitable_variables]
      ))
      shiny::updateSelectInput(
        session,
        "stratified_by",
        choices = choices,
        selected = "None"
      )
    })
    ## Updates Fisher test to "two sided" if stratify by variable is selected
    shiny::observeEvent(input$stratified_by, {
      shiny::req(input$stratified_by)
      if (input$stratified_by != "None") {
        choices <- list("Two sided" = "two.sided")
      } else if (input$stratified_by == "None") {
        choices <- list(
          "Two sided" = "two.sided",
          "One sided" = "less"
        )
      }
      shiny::updateSelectInput(
        session,
        "test_alternative",
        choices = choices
      )
    })
    ## Filter of minimum number of events ----
    min_events <- shiny::eventReactive(
      list(r$filtered_data, input$alpha, input$test_alternative),
      {
        shiny::req(r$filtered_data, input$alpha, input$test_alternative)
        n_data <- r$filtered_data |>
          dplyr::distinct(.data$USUBJID, .data$trta_detector) |>
          dplyr::count(.data$trta_detector, name = "big_n")
        n_verum <- n_data |>
          dplyr::filter(.data$trta_detector == "Verum") |>
          dplyr::pull("big_n")
        n_comparator <- n_data |>
          dplyr::filter(.data$trta_detector == "Comparator") |>
          dplyr::pull("big_n")
        find_min_event(
          n1 = n_verum,
          n2 = n_comparator,
          alternative = input$test_alternative,
          alpha = input$alpha
        )
      }
    )
    shiny::observeEvent(min_events(), {
      me <- min_events()
      # Only add "min" choice when a finite minimum exists
      if (is.finite(me) && length(me) > 0) {
        min_events_label <- paste0(
          "Use the minimum number of events in Verum (n = ",
          me,
          ")"
        )
        choices <- list("no_method", "one_percent", "min")
        names(choices) <- c(
          "No method",
          "Overall incidence >= 1%",
          min_events_label
        )
      } else {
        choices <- list("no_method", "one_percent")
        names(choices) <- c(
          "No method",
          "Overall incidence >= 1%"
        )
      }
      shiny::updateSelectInput(
        session,
        "aes_filter",
        choices = choices
      )
    })
    # AE grouping filter ----
    ae_grouping_filter_variable <- shiny::reactive({
      if (calculate_mode == "heatmap") {
        # For heatmap, only filter by SOCs
        "AEBODSYS"
      } else {
        input$safety_variable
      }
    })
    choices_ae_grouping_filter <- shiny::reactive({
      shiny::req(ae_grouping_filter_variable())
      variable <- ae_grouping_filter_variable()
      # Select joint ADAE-ADSL-MedDRA datasets
      if (variable == "SMQ_NAME") {
        shiny::req(r$filtered_data_smq)
        joint_data <- r$filtered_data_smq
      } else if (variable == "ocmq") {
        shiny::req(r$filtered_data_ocmq)
        joint_data <- r$filtered_data_ocmq
      } else if (r$meddra_mode == "with_meddra") {
        shiny::req(r$filtered_data_mlg)
        joint_data <- r$filtered_data_mlg
      } else {
        shiny::req(r$filtered_data)
        joint_data <- r$filtered_data
      }
      choices <- joint_data |>
        dplyr::filter(!is.na(.data[[variable]])) |>
        dplyr::pull(variable) |>
        unique() |>
        sort()
      choices
    })
    shiny::observe({
      shiny::req(choices_ae_grouping_filter())
      choices <- choices_ae_grouping_filter()
      if (calculate_mode == "heatmap" || input$safety_variable == "AEBODSYS") {
        label <- "System Organ Classes (SOCs)"
      } else if (input$safety_variable == "AEDECOD") {
        label <- "Preferred Terms (PTs)"
      } else if (input$safety_variable == "MLG_label") {
        label <- "Medical Labeling Groupings (MLGs)"
      } else if (input$safety_variable == "SMQ_NAME") {
        label <- "Standardised MedDRA Queries (SMQs)"
      } else if (input$safety_variable == "ocmq") {
        label <- "Office of New Drugs Custom Medical Queries (OCMQs)"
      }
      shinyWidgets::updatePickerInput(
        session,
        "ae_grouping_filter",
        label = label,
        choices = choices,
        selected = choices
      )
    }) |>
      shiny::bindEvent(choices_ae_grouping_filter())
    # Calculate data ----
    ## Common calculations for all tabs ----
    results_all <- shiny::reactive({
      shiny::req(
        r$filtered_data,
        input$frequency_measure,
        input$stratified_by,
        input$alpha,
        input$test_alternative,
        input$aes_filter,
        input$ae_type_filter,
        r$treatment_emergent_flag_variable,
        r$treatment_emergent_flag_value,
        r$serious_flag_variable,
        r$serious_flag_value
      )
      # Pass arguments ----
      filtered_data <- r$filtered_data
      adsl_filtered_data <- r$adsl_filtered
      frequency_measure <- input$frequency_measure
      study_strat <- input$stratified_by
      alpha <- as.numeric(input$alpha)
      alternative <- input$test_alternative
      filter <- input$aes_filter
      ae_type_filter <- input$ae_type_filter
      treatment_emergent_variable <- r$treatment_emergent_flag_variable
      treatment_emergent_value <- r$treatment_emergent_flag_value
      serious_variable <- r$serious_flag_variable
      serious_value <- r$serious_flag_value
      drug_related_variable <- r$drug_related_flag_variable
      drug_related_value <- r$drug_related_flag_value
      # Effect_measure (RR/RD), order by (p-value/effect) and adjustment method
      # have different logic for each plot type
      ## Heatmap ----
      if (calculate_mode == "heatmap") {
        shiny::req(input$heatmap_variable, input$heatmap_color)
        heatmap_variable <- input$heatmap_variable
        heatmap_color <- input$heatmap_color
        variable <- "AEDECOD"
        effect_measure <- dplyr::case_when(
          heatmap_color %in% c("RR", "FDR", "DFDR") ~ "RR",
          heatmap_color %in% c("RD") ~ "RD",
        )
        order_by <- dplyr::case_when(
          heatmap_color %in% c("RR", "RD") ~ "effect",
          heatmap_color %in% c("FDR", "DFDR") ~ "p-value",
        )
        adjustment <- dplyr::case_when(
          heatmap_color %in% c("RR", "RD", "FDR") ~ "FDR",
          heatmap_color %in% c("DFDR") ~ "DFDR",
        )
      } else if (calculate_mode == "volcano") {
        ## Volcano ----
        shiny::req(input$effect_measure, input$safety_variable)
        variable <- input$safety_variable
        effect_measure <- input$effect_measure
        order_by <- "p-value"
        adjustment <- "FDR"
      } else if (calculate_mode == "double_dot") {
        ## Double dot ----
        shiny::req(
          input$safety_variable,
          input$adjustment_method,
          input$order_by,
          input$effect_measure
        )
        variable <- input$safety_variable
        effect_measure <- input$effect_measure
        order_by <- input$order_by
        adjustment <- input$adjustment_method
      } else if (calculate_mode == "table") {
        ## Data table ----
        shiny::req(input$safety_variable, input$adjustment_method)
        variable <- input$safety_variable
        effect_measure <- "RR"
        order_by <- "p-value"
        adjustment <- input$adjustment_method
      }
      shiny::withProgress(value = 0, message = "Calculating results...", {
        # Select joint ADAE-ADSL-MedDRA datasets ----
        if (variable == "SMQ_NAME") {
          shiny::req(r$filtered_data_smq)
          joint_data <- r$filtered_data_smq
        } else if (variable == "ocmq") {
          shiny::req(r$filtered_data_ocmq)
          joint_data <- r$filtered_data_ocmq
        } else if (r$meddra_mode == "with_meddra") {
          shiny::req(r$filtered_data_mlg)
          joint_data <- r$filtered_data_mlg
        } else {
          joint_data <- filtered_data
        }
        logger::log_info(
          "joint_data: dim: {paste(dim(joint_data), collapse = ', ')}"
        )

        # Filter by AE type ----
        joint_data_ae_type <- joint_data |>
          filter_ae_type(
            type = ae_type_filter,
            treatment_emergent_variable = treatment_emergent_variable,
            treatment_emergent_value = treatment_emergent_value,
            serious_variable = serious_variable,
            serious_value = serious_value,
            drug_related_variable = drug_related_variable,
            drug_related_value = drug_related_value
          )
        logger::log_info(
          "joint_data_ae_type: dim: {paste(dim(joint_data_ae_type), collapse = ', ')}"
        )

        # Avoid errors due to 0 rows dataset
        validate_need(
          nrow(joint_data_ae_type) > 0,
          "Filtered data has zero rows!"
        )

        # Calculate results ----
        if (study_strat != "None") {
          shiny::setProgress(
            detail = "Performing stratified analysis, this may take a while."
          )
        }
        data_results <- calculate_results(
          joint_data = joint_data_ae_type,
          adsl_filtered_data = adsl_filtered_data,
          variable = variable,
          effect_measure = effect_measure,
          adjustment = adjustment,
          order_by = order_by,
          study_strat = study_strat,
          alternative = alternative,
          alpha = alpha,
          filter = filter,
          frequency_measure = frequency_measure,
          duration_mode = r$duration_mode,
          ae_duration_variable = r$ae_duration_variable,
          exposure_duration_variable = r$exposure_duration_variable,
          exposure_start_variable = r$exposure_start_variable,
          exposure_end_variable = r$exposure_end_variable,
          ae_start_variable = r$ae_start_variable
        )
      })
      logger::log_info(
        "data_results: dim: {paste(dim(data_results), collapse = ', ')}"
      )
      data_results
    }) |>
      shiny::bindEvent(input$go_calculate)
    shiny::observeEvent(results_all(), {
      shiny::req(input$go_calculate)
      bslib::update_task_button(
        id = "go_calculate",
        state = "ready",
        session = session
      )
    })

    if (calculate_mode == "double_dot") {
      ## Arrange and filter double dot plot data ----
      double_dot_data <- shiny::reactive({
        shiny::req(
          results_all(),
          r$filtered_data,
          input$safety_variable,
          input$effect_measure,
          input$adjustment_method,
          input$order_by,
          input$number_aes_shown,
          input$ae_grouping_filter
        )
        # Early check if filtered data is empty
        if (nrow(results_all()) == 0) {
          # Return empty tibble
          tibble::tibble()
        } else {
          # Pass arguments ----
          variable <- input$safety_variable
          effect_measure <- input$effect_measure
          adjustment <- input$adjustment_method
          order_by <- input$order_by
          number_aes <- as.numeric(input$number_aes_shown)
          ae_grouping_filter <- input$ae_grouping_filter
          # Get results ----
          logger::log_info(
            "results_all: dim: {paste(dim(results_all()), collapse = ', ')}"
          )
          # Also including OVERALL category
          if (input$include_overall) {
            ae_grouping <- c(ae_grouping_filter, "OVERALL")
          } else {
            ae_grouping <- ae_grouping_filter
          }
          data_results <- results_all() |>
            # Filter AE categories (quick filter)
            dplyr::filter(
              .data[[variable]] %in% ae_grouping
            )
          logger::log_info(
            "data_results: dim: {paste(dim(data_results), collapse = ', ')}"
          )

          if (nrow(data_results) == 0) {
            tibble::tibble()
          } else {
            # Sort results ----
            # Arrange factor levels order to plot according to custom ordering
            data_reordered_levels <- reorder_levels(
              data = data_results,
              variable = variable,
              order_by = order_by,
              effect_measure = effect_measure,
              adjustment = adjustment
            )
            logger::log_info(
              "data_reordered_levels: dim: {paste(dim(data_reordered_levels), collapse = ', ')}"
            )
            # Reorder data rows to show all plots in the appropiate order
            data_arranged <- arrange_data(
              data = data_reordered_levels,
              order_by = order_by,
              adjustment = adjustment,
              effect_measure = effect_measure,
              number_aes = number_aes
            )
            logger::log_info(
              "data_arranged: dim: {paste(dim(data_arranged), collapse = ', ')}"
            )
            data_arranged
          }
        }
      }) |>
        shiny::bindEvent(results_all())
    } else if (calculate_mode == "volcano") {
      ## Calculate volcano data ----
      volcano_data <- shiny::reactive({
        shiny::req(
          results_all(),
          input$safety_variable,
          input$effect_measure,
          input$alpha,
          input$ae_grouping_filter
        )
        # Early check if filtered data is empty
        if (nrow(results_all()) == 0) {
          # Return empty tibble
          tibble::tibble()
        } else {
          # Pass arguments ----
          variable <- input$safety_variable
          effect_measure <- input$effect_measure
          alpha <- as.numeric(input$alpha)
          ae_grouping_filter <- input$ae_grouping_filter
          # Get results ----
          data_results <- results_all() |>
            # Filter AE categories (quick filter)
            dplyr::filter(.data[[variable]] %in% ae_grouping_filter)
          if (nrow(data_results) == 0) {
            tibble::tibble()
          } else {
            # Prepare volcano data ----
            data_volcano <- prepare_volcano(
              data = data_results,
              safety_variable = variable
            )
            data_volcano_colors <- color_volcano(
              data = data_volcano,
              effect_measure = effect_measure,
              alpha = alpha
            )
            data_volcano_colors
          }
        }
      }) |>
        shiny::bindEvent(results_all())
    } else if (calculate_mode == "heatmap") {
      ## Calculate heatmap data ----
      heatmap_data <- shiny::eventReactive(results_all(), {
        shiny::req(
          results_all(),
          r$adae_data,
          input$heatmap_variable,
          input$heatmap_color,
          input$alpha,
          input$ae_grouping_filter
        )
        # Early check if filtered data is empty
        if (nrow(results_all()) == 0) {
          # Return empty tibble
          tibble::tibble()
        } else {
          # Pass arguments ----
          adae_data <- r$adae_data
          heatmap_variable <- input$heatmap_variable
          heatmap_color <- input$heatmap_color
          alpha <- as.numeric(input$alpha)
          ae_grouping_filter <- input$ae_grouping_filter
          # Calculate double dot plot data for PT as lower level ----
          results <- results_all() |>
            # Filter AE categories (quick filter)
            dplyr::filter(.data$AEBODSYS %in% ae_grouping_filter)
          if (nrow(results) == 0) {
            tibble::tibble()
          } else {
            # Get heatmap data ----
            prepared_data <- prepare_heatmap_data(
              data = results,
              heatmap_variable = heatmap_variable,
              heatmap_color = heatmap_color,
              adae_data = adae_data
            )
            heatmap_counts <- count_heatmap_data(
              data = prepared_data,
              heatmap_variable = heatmap_variable
            )
            if (NROW(heatmap_counts) > 0) {
              treemap_data <- make_treemap_data(
                data = prepared_data,
                data_counts = heatmap_counts,
                heatmap_variable = heatmap_variable,
                heatmap_color = heatmap_color,
                alpha = alpha
              )
              treemap_data
            } else {
              tibble::tibble() # empty data frame when no DFDR significant p-values
            }
          }
        }
      })
    } else if (calculate_mode == "table") {
      # Calculate table data ----
      table_data <- shiny::eventReactive(results_all(), {
        shiny::req(
          results_all(),
          input$safety_variable,
          input$ae_grouping_filter
        )
        # Early check if filtered data is empty
        if (nrow(results_all()) == 0) {
          # Return empty tibble
          tibble::tibble()
        } else {
          # Pass arguments ----
          variable <- input$safety_variable
          ae_grouping_filter <- input$ae_grouping_filter
          data_results <- results_all() |>
            # Filter AE categories (quick filter)
            dplyr::filter(.data[[variable]] %in% ae_grouping_filter)
          if (nrow(data_results) == 0) {
            tibble::tibble()
          } else {
            data_results
          }
        }
      })
    }
    # Debug ----
    output$debug <- shiny::renderPrint({
      golem::cat_dev(
        paste("DEBUG MODE"),
        paste("---- input variables ----"),
        paste("input$go_calculate =", input$go_calculate),
        paste("input$safety_variable =", input$safety_variable),
        paste(
          "input$ae_grouping_filter exists",
          !is.null(input$ae_grouping_filter)
        ),
        paste("input$frequency_measure =", input$frequency_measure),
        paste("input$effect_measure =", input$effect_measure),
        paste("input$order_by =", input$order_by),
        paste("input$adjustment_method =", input$adjustment_method),
        paste("input$stratified_by =", input$stratified_by),
        paste(
          "input$switch_advanced_settings =",
          input$switch_advanced_settings
        ),
        paste("input$number_aes_shown =", input$number_aes_shown),
        paste("input$test_alternative =", input$test_alternative),
        paste("input$alpha =", input$alpha),
        paste("input$aes_filter =", input$aes_filter),
        paste("input$show_full_labels =", input$show_full_labels),
        paste("input$heatmap_variable =", input$heatmap_variable),
        paste("input$heatmap_color =", input$heatmap_color),
        paste("---- reactives -----"),
        paste("go_plot() =", go_plot()),
        paste("results_all() nrow =", nrow(results_all())),
        paste(
          "results_all() nrow =",
          nrow(results_all())
        ),
        paste(
          "choices_ae_grouping_filter() exists",
          !is.null(choices_ae_grouping_filter())
        ),
        paste("---- r global variables ----"),
        paste("r$filtered_data exists", !is.null(r$filtered_data)),
        paste("r$go_double_dot =", paste(r$go_double_dot, collapse = ", ")),
        paste("r$go_heatmap =", r$go_heatmap),
        paste("r$go_volcano =", r$go_volcano),
        paste("r$go_table =", r$go_table),
        paste("r$alpha =", r$alpha),
        paste("r$double_dot_data exists", !is.null(r$double_dot_data)),
        paste("r$heatmap_data exists", !is.null(r$heatmap_data)),
        paste("r$volcano_data exists", !is.null(r$volcano_data)),
        paste("r$table_data exists", !is.null(r$table_data)),
        paste("r$frequency_measure =", r$frequency_measure),
        paste("r$effect_measure =", r$effect_measure),
        paste("r$adjustment_method =", r$adjustment_method),
        paste("r$stratified_by =", r$stratified_by),
        paste("r$label_collapsed =", r$label_collapsed),
        paste("r$heatmap_color =", r$heatmap_color),
        paste("r$theme =", r$theme),
        paste(
          "r$treatment_emergent_flag_variable =",
          r$treatment_emergent_flag_variable
        ),
        paste(
          "r$treatment_emergent_flag_value =",
          r$treatment_emergent_flag_value
        ),
        sep = "\n"
      )
    })
    # Pass objects to reactiveValues list ----
    # Reactive to reload plotting
    go_plot <- shiny::reactive(input$go_calculate)
    # Different reactives to ensure that each tab only updates its own plot
    if (calculate_mode == "double_dot") {
      shiny::observe({
        r$double_dot_data <- double_dot_data()
      })
      shiny::observe({
        r$label_collapsed <- !input$show_full_labels
      })
      shiny::observe({
        r$stratified_by <- input$stratified_by
      })
      shiny::observe({
        r$go_double_dot <- go_plot()
      })
    } else if (calculate_mode == "volcano") {
      shiny::observe({
        r$volcano_data <- volcano_data()
      })
      shiny::observe({
        r$go_volcano <- go_plot()
      })
    } else if (calculate_mode == "heatmap") {
      shiny::observe({
        r$heatmap_data <- heatmap_data()
      })
      shiny::observe({
        r$heatmap_color <- input$heatmap_color
      })
      shiny::observe({
        r$go_heatmap <- go_plot()
      })
    } else if (calculate_mode == "table") {
      shiny::observe({
        r$table_data <- table_data()
      })
      shiny::observe({
        r$table_variable <- input$safety_variable
      })
      shiny::observe({
        r$adjustment_method <- input$adjustment_method
      })
      shiny::observe({
        r$go_table <- go_plot()
      })
    }
    # Objects passed to several modules
    if (calculate_mode %in% c("double_dot", "volcano")) {
      shiny::observe({
        r$frequency_measure <- input$frequency_measure
      })
      shiny::observe({
        r$effect_measure <- input$effect_measure
      })
      shiny::observe({
        r$adjustment_method <- input$adjustment_method
      })
    }
    # Objects passed to all graphic modules
    shiny::observe({
      r$alpha <- as.numeric(input$alpha)
    })
  })
}
