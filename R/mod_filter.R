#' filter UI Function
#'
#' @description A shiny Module.
#'
#' @inheritParams mod_upload_ui
#'
#' @noRd

mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_fillable(
    shiny::verbatimTextOutput(ns("debug")), # uncomment to show debug prints
    bslib::accordion(
      bslib::accordion_panel(
        "Add or remove filters",
        icon = shiny::icon("filter"),
        # Select ADAE filters ----
        shinyWidgets::pickerInput(
          ns("picker_filter_adae"),
          "Select filter variable(s) for ADAE data set",
          choices = NULL,
          multiple = TRUE,
          options = picker_input_options()
        ),
        # Select ADSL filters ----
        shinyWidgets::pickerInput(
          ns("picker_filter_adsl"),
          "Select filter variable(s) for ADSL data set",
          choices = NULL,
          multiple = TRUE,
          options = picker_input_options()
        ),
        # Remove filter button ----
        shiny::actionButton(
          ns("remove_filter"),
          "Remove all filters",
          icon = shiny::icon("remove"),
          width = "100%",
          class = "button-error"
        ),
        shiny::br(),
        # Show filters ----
        shiny::textOutput(ns("no_filters")),
        shiny::conditionalPanel(
          condition = "!output.no_filters",
          ns = ns,
          # Placeholder where filter buttons will be added
          id = "placeholder"
        ),
        # Filter data buttons ----
        shiny::actionButton(
          ns("go_filter_1"),
          "Filter data!",
          icon = shiny::icon("filter"),
          width = "100%"
        )
      )
    ),
    # Show dataset info ----
    bslib::accordion(
      bslib::accordion_panel(
        "Dataset information",
        icon = shiny::icon("info"),
        mod_info_ui(paste0(id, "_info"))
      )
    )
  )
}

#' filter Server Functions
#' @inheritParams mod_upload_server
#' @noRd
mod_filter_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Filter options ADAE ----
    shiny::observeEvent(r$unfiltered_data, {
      shiny::req(
        r$unfiltered_data,
        r$adae_variable_names,
        r$adsl_variable_names
      )
      # Select only variables that are on ADAE and not on ADSL
      adae_variables <- r$adae_variable_names
      adsl_variables <- r$adsl_variable_names
      # Remove AE type filters (TEAEs, SAEs, Drug-related), as these are now
      # chosen directly on mod_calculate
      ae_type_filters <- c(
        r$treatment_emergent_flag_variable,
        r$serious_flag_variable,
        r$drug_related_flag_variable
      )
      choices <- adae_variables[
        !(adae_variables %in% c(adsl_variables, ae_type_filters))
      ]
      shinyWidgets::updatePickerInput(
        session,
        "picker_filter_adae",
        choices = choices
      )
    })
    # Filter options ADSL ----
    shiny::observeEvent(r$treatment_variable, {
      shiny::req(r$adsl_variable_names, r$treatment_variable)
      choices <- r$adsl_variable_names
      choices <- choices[choices != r$treatment_variable]
      shinyWidgets::updatePickerInput(
        session,
        "picker_filter_adsl",
        choices = choices
      )
    })
    # Manage filters ----
    # Compare the selected filter variables with the existing filter inputs:
    # Keep the existing filter inputs without changing them
    # Add new filter inputs
    # Remove filter inputs no longer selected
    active_filters <- shiny::reactiveVal()
    selected_filters <- shiny::reactive({
      filter_adae_adsl <- c(
        input$picker_filter_adae,
        input$picker_filter_adsl
      )
      if (!is.null(filter_adae_adsl)) {
        paste0("filter_", filter_adae_adsl)
      }
    })
    output$no_filters <- shiny::renderText({
      if (is.null(active_filters())) {
        "No filters added!"
      }
    })
    # Update filter buttons ----
    shiny::observeEvent(
      selected_filters(),
      ignoreNULL = FALSE,
      {
        shiny::req(r$adae_data, r$adsl_data)
        adae_data <- r$adae_data
        adsl_data <- r$adsl_data
        adae_names <- r$adae_variable_names
        adsl_names <- r$adsl_variable_names
        selected_filters <- selected_filters()
        active_filters <- active_filters()
        new_filters <- selected_filters[
          !selected_filters %in% active_filters
        ]
        deselected_filters <- active_filters[
          !active_filters %in% selected_filters
        ]
        ## Remove buttons for deselected filters ----
        for (i in seq_along(deselected_filters)) {
          id <- deselected_filters[i]
          shiny.destroy::removeInput(id)
        }
        ## Add buttons for new filter variables ----
        for (i in seq_along(new_filters)) {
          id <- new_filters[i]
          variable_name <- stringr::str_remove(id, "^filter_")
          if (variable_name %in% colnames(adae_data)) {
            variable <- adae_data |> dplyr::pull(variable_name)
            variable_label <- adae_names[adae_names == variable_name] |>
              names()
          } else if (variable_name %in% colnames(adsl_data)) {
            variable <- adsl_data |> dplyr::pull(variable_name)
            variable_label <- adsl_names[adsl_names == variable_name] |>
              names()
          }
          shiny::insertUI(
            selector = "#placeholder",
            ui = shiny::tags$div(
              # Treatment-emergent flag selected by default
              if (variable_name == r$treatment_emergent_flag_variable) {
                choices <- unique(variable)
                shinyWidgets::pickerInput(
                  inputId = ns(id),
                  label = variable_label,
                  choices = choices,
                  selected = r$treatment_emergent_flag_value,
                  multiple = TRUE,
                  options = picker_input_options()
                )
              } else if (!is.numeric(variable)) {
                # With non-numeric variables, use pickerInput
                choices <- unique(variable)
                shinyWidgets::pickerInput(
                  inputId = ns(id),
                  label = variable_label,
                  choices = choices,
                  selected = choices,
                  multiple = TRUE,
                  options = picker_input_options()
                )
              } else if (is.numeric(variable)) {
                # With numeric variables, use sliderInput
                min_value <- min(variable, na.rm = TRUE)
                max_value <- max(variable, na.rm = TRUE)
                if (is.integer(variable)) {
                  # With integer variables, use sliderInput with steps of 1
                  step <- 1
                  sep <- ""
                  ticks <- FALSE
                } else if (!is.integer(variable)) {
                  # With continuous variables, default values
                  step <- NULL
                  sep <- ","
                  ticks <- TRUE
                }
                shiny::sliderInput(
                  inputId = ns(id),
                  label = variable_name,
                  value = c(min_value, max_value),
                  min = min_value,
                  max = max_value,
                  step = step,
                  sep = sep,
                  ticks = ticks
                )
              },
              id = id,
              class = "detector-filter"
            )
          )
        }
        ## Update active filters ----
        active_filters(selected_filters())
      }
    )
    # Remove filter button ----
    shiny::observeEvent(input$remove_filter, {
      # Remove filters from UI
      for (i in seq_along(active_filters())) {
        id <- active_filters()[i]
        shiny.destroy::removeInput(id)
      }
      # Remove filter from reactive value of active filters
      active_filters(NULL)
      # Empty filter selectors for ADAE and ADSL
      shinyWidgets::updatePickerInput(
        session,
        "picker_filter_adae",
        selected = character(0)
      )
      shinyWidgets::updatePickerInput(
        session,
        "picker_filter_adsl",
        selected = character(0)
      )
    })
    # Filter ADAE data ----
    adae_filtered <- shiny::eventReactive(
      c(input$go_filter_1, input$go_filter_2, r$adae_data),
      ignoreNULL = FALSE,
      {
        shiny::req(r$adae_data)
        data <- r$adae_data
        if (!is.null(active_filters())) {
          ids <- active_filters() # Filter input ids
          variables <- stringr::str_remove(ids, "^filter_") # Variable names
          # Initialize starting data
          data_filt <- data
          # Filtering using a for loop, one iteration per filter condition
          for (i in seq_along(ids)) {
            id <- ids[i]
            variable_name <- variables[i]
            if (variable_name %in% colnames(data)) {
              variable <- data |> dplyr::pull(variable_name)
              if (is.numeric(variable)) {
                # Logic for numerical variables
                min_value <- input[[id]][1]
                max_value <- input[[id]][2]
                data_filt <- data_filt |>
                  dplyr::filter(
                    dplyr::between(.data[[variable_name]], min_value, max_value)
                  )
              } else {
                # Logic for character variables
                selected_values <- input[[id]]
                data_filt <- data_filt |>
                  dplyr::filter(.data[[variable_name]] %in% selected_values)
              }
            }
          }
          data_filt
        } else {
          data
        }
      }
    )
    # Filter ADSL data ----
    adsl_filtered <- shiny::eventReactive(
      c(input$go_filter_1, input$go_filter_2, r$adsl_data),
      ignoreNULL = FALSE,
      {
        shiny::req(r$adsl_data)
        data <- r$adsl_data
        if (!is.null(active_filters())) {
          ids <- active_filters() # Filter input ids
          variables <- stringr::str_remove(ids, "^filter_") # Variable names
          # Initialize starting data
          data_filt <- data
          # Filtering using a for loop, one iteration per filter condition
          for (i in seq_along(ids)) {
            id <- ids[i]
            variable_name <- variables[i]
            if (variable_name %in% colnames(data)) {
              # Logic for numerical variables
              variable <- data |> dplyr::pull(variable_name)
              if (is.numeric(variable)) {
                min_value <- input[[id]][1]
                max_value <- input[[id]][2]
                data_filt <- data_filt |>
                  dplyr::filter(
                    dplyr::between(.data[[variable_name]], min_value, max_value)
                  )
              } else {
                # Logic for character variables
                selected_values <- input[[id]]
                data_filt <- data_filt |>
                  dplyr::filter(.data[[variable_name]] %in% selected_values)
              }
            }
          }
          data_filt
        } else {
          data
        }
      }
    )
    # Join ADAE - ADSL filtered ----
    filtered_data <- shiny::eventReactive(
      c(adsl_filtered(), adae_filtered()),
      {
        shiny::req(adsl_filtered(), adae_filtered())
        # If any of ADAE or ADSL has 0 rows after filtering, store as NULL,
        # that will trigger an error message
        if (nrow(adsl_filtered()) > 0 && nrow(adae_filtered()) > 0) {
          join_adsl_adae(adsl = adsl_filtered(), adae = adae_filtered())
        } else {
          NULL
        }
      }
    )
    # Join filtered ADAE-ADSL with MedDRA datasets ----
    ## HLGT/HLT and MLG ----
    filtered_data_mlg <- shiny::reactive({
      shiny::req(filtered_data(), r$meddra_data, r$mlg_data)
      filtered_data() |>
        # Add HLT/HLGT
        join_meddra_data(
          mode = "hlt",
          meddra_data = r$meddra_data
        ) |>
        # Add MLG
        join_meddra_data(
          mode = "mlg",
          meddra_data = r$mlg_data
        )
    }) |>
      shiny::bindEvent(filtered_data())
    ## SMQ ----
    filtered_data_smq <- shiny::reactive({
      shiny::req(filtered_data(), r$smq_data)
      filtered_data() |>
        join_meddra_data(
          mode = "smq",
          meddra_data = r$smq_data
        )
    }) |>
      shiny::bindEvent(filtered_data())
    ## OCMQ ----
    filtered_data_ocmq <- shiny::reactive({
      shiny::req(filtered_data())
      filtered_data() |>
        join_ocmq_data()
    }) |>
      shiny::bindEvent(filtered_data())
    # List of active filters to show it on the UI ----
    filter_list <- shiny::eventReactive(
      c(input$go_filter_1, input$go_filter_2),
      ignoreNULL = FALSE,
      {
        # One separate list for ADAE and ADSl filters
        filter_list <- list(adae = c(), adsl = c())
        # Starting value is NULL, to show "None" on the UI
        if (all(c(input$go_filter_1, input$go_filter_2) == 0)) {
          filter_list$adsl <- NULL
          filter_list$adae <- NULL
        } else {
          adae_index <- 1
          adsl_index <- 1
          # Extracts selected values for each filtered variable
          for (i in seq_along(active_filters())) {
            id <- active_filters()[i]
            variable_name <- stringr::str_remove(id, "^filter_")
            if (variable_name %in% colnames(r$adae_data)) {
              data <- r$adae_data
              list <- "adae"
              index <- adae_index
            } else if (variable_name %in% colnames(r$adsl_data)) {
              data <- r$adsl_data
              list <- "adsl"
              index <- adsl_index
            }
            variable <- data |> dplyr::pull(variable_name)
            if (is.numeric(variable)) {
              filter_list[[list]][[index]] <- stringr::str_glue(
                "({variable_name}: [{input[[id]][1]}-{input[[id]][2]}])"
              )
              if (list == "adae") {
                adae_index <- adae_index + 1
              } else {
                adsl_index <- adsl_index + 1
              }
            } else if (!identical(input[[id]], unique(variable))) {
              # If it's a character variable, omit if you've selected all
              # possible values
              filter_list[[list]][[index]] <- stringr::str_glue(
                "({variable_name}: {paste(input[[id]], collapse = ', ')})"
              )
              if (list == "adae") {
                adae_index <- adae_index + 1
              } else {
                adsl_index <- adsl_index + 1
              }
            }
          }
        }
        filter_list
      }
    )
    filter_list_adae <- shiny::reactive({
      filter_list <- filter_list()
      if (!is.null(filter_list$adae)) {
        adae_list <- paste0(
          "ADAE: ",
          paste(filter_list$adae, collapse = ", "),
          "."
        )
      } else {
        adae_list <- "ADAE: none."
      }
      adae_list
    })
    filter_list_adsl <- shiny::reactive({
      if (!is.null(filter_list()) && !is.null(filter_list()$adsl)) {
        adsl_list <- paste0(
          "ADSL: ",
          paste(filter_list()$adsl, collapse = ", "),
          "."
        )
      } else {
        adsl_list <- "ADSL: none."
      }
      adsl_list
    })
    output$filter_list_adae <- shiny::renderText(filter_list_adae())
    output$filter_list_adsl <- shiny::renderText(filter_list_adsl())
    # Move to next page ----
    shiny::observeEvent(input$next_graph, {
      shinydashboard::updateTabItems(
        session = r$parent_session,
        inputId = "tabs",
        selected = "graph"
      )
    })
    # Return values to reactiveValues "r" to communicate with other modules ----
    shiny::observe({
      r$filtered_data <- filtered_data()
    })
    shiny::observe({
      r$filtered_data_mlg <- filtered_data_mlg()
    })
    shiny::observe({
      r$filtered_data_smq <- filtered_data_smq()
    })
    shiny::observe({
      r$filtered_data_ocmq <- filtered_data_ocmq()
    })
    shiny::observe({
      r$filter_list_adae <- filter_list_adae()
    })
    shiny::observe({
      r$filter_list_adsl <- filter_list_adsl()
    })
    shiny::observe({
      r$adae_filtered <- adae_filtered()
    })
    shiny::observe({
      r$adsl_filtered <- adsl_filtered()
    })
    # Debug ----
    output$debug <- shiny::renderPrint({
      golem::cat_dev(
        paste("DEBUG MODE"),
        paste("---- reactive variables ----"),
        paste(
          "adae_filtered colnames =",
          paste(colnames(adae_filtered()), collapse = ", ")
        ),
        paste(
          "adsl_filtered colnames =",
          paste(colnames(adsl_filtered()), collapse = ", ")
        ),
        paste("active_filters =", paste(active_filters(), collapse = ", ")),
        paste("selected_filters =", paste(selected_filters(), collapse = ", ")),
        paste("---- r global variables ----"),
        paste("r$unfiltered_data exists", !is.null(r$unfiltered_data)),
        paste("r$filtered_data exists", !is.null(r$filtered_data)),
        paste(
          "r$unfiltered_data dims =",
          paste(dim(r$unfiltered_data), collapse = ", ")
        ),
        paste(
          "r$filtered_data dims =",
          paste(dim(r$filtered_data), collapse = ", ")
        ),
        paste(
          "r$adae_data dims =",
          paste(dim(r$adae_data), collapse = ", ")
        ),
        paste(
          "r$adae_filtered dims =",
          paste(dim(r$adae_filtered), collapse = ", ")
        ),
        paste(
          "r$adsl_data dims =",
          paste(dim(r$adsl_data), collapse = ", ")
        ),
        paste(
          "r$adsl_filtered dims =",
          paste(dim(r$adsl_filtered), collapse = ", ")
        ),
        paste(
          "filtered_data_mlg colnames =",
          paste(colnames(r$filtered_data_mlg), collapse = ", ")
        ),
        paste(
          "filtered_data_smq colnames =",
          paste(colnames(r$filtered_data_smq), collapse = ", ")
        ),
        paste(
          "filtered_data_ocmq colnames =",
          paste(colnames(r$filtered_data_ocmq), collapse = ", ")
        ),
        sep = "\n"
      )
    })
  })
}
