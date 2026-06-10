#' upload module UI
#'
#' A shiny Module for Data upload panel
#'
#' @param id Internal parameters for shiny.
#'
mod_upload_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::conditionalPanel(
      condition = "!output.upload_ready",
      ns = ns,
      # Row: ADAE-ADSL files ----
      shiny::wellPanel(
        class = "upload",
        ## Demo or files mode ----
        flex_row(
          shiny::radioButtons(
            ns("mode"),
            "Input mode:",
            choices = c(
              "Upload ADAM Files" = "sas",
              "Demo Data" = "demo"
            )
          )
        ),
        ## Upload ADAE and ADSL files----
        shiny::conditionalPanel(
          condition = "input.mode == 'sas'",
          ns = ns,
          class = "flex-row",
          shiny::div(
            shiny::fileInput(
              ns("adae_file"),
              "Adverse Event data (ADAE)",
              accept = c(".sas7bdat", ".rds", ".csv")
            ),
            shiny::textOutput(ns("adae_missing")),
            shiny::textOutput(ns("adae_check"))
          ),
          shiny::div(
            shiny::fileInput(
              ns("adsl_file"),
              "Subject-level  data  (ADSL)",
              accept = c(".sas7bdat", ".rds", ".csv")
            ),
            shiny::textOutput(ns("adsl_missing")),
            shiny::textOutput(ns("adsl_check"))
          )
        )
      ),
      # Row: MedDRA data ----
      shiny::wellPanel(
        class = "upload",
        ## Run with or without MedDRA ----
        flex_row(
          shiny::radioButtons(
            ns("meddra_mode"),
            "Choose MedDRA mode:",
            choices = c(
              "Upload MedDRA data" = "with_meddra",
              "Run without MedDRA" = "without_meddra"
            )
          )
        ),
        ## Upload MedDRA files and select MedDRA version ----
        shiny::conditionalPanel(
          condition = "input.meddra_mode == 'with_meddra'",
          ns = ns,
          class = "flex-row",
          shiny::div(
            shiny::fileInput(
              ns("meddra_file"),
              label = "Upload MedDRA file",
              accept = c(".sas7bdat", ".rds", ".rdta", ".csv")
            ),
            shiny::textOutput(ns("meddra_missing")),
            shiny::textOutput(ns("meddra_check"))
          ),
          shiny::div(
            shiny::fileInput(
              ns("smq_file"),
              label = "Upload MedDRA SMQ view file",
              accept = c(".sas7bdat", ".rds", ".rdta", ".csv")
            ),
            shiny::textOutput(ns("smq_missing")),
            shiny::textOutput(ns("smq_check"))
          ),
          shiny::div(
            shiny::selectInput(
              ns("meddra_version"),
              label = "Select MedDRA version",
              choices = c("Please upload MedDRA file" = "")
            )
          )
        )
      ),
      # Confirm data upload button ----
      shiny::actionButton(
        ns("confirm_upload"),
        "Confirm upload",
        icon = shiny::icon("check"),
        class = "btn-lg align-center"
      )
    ),
    invisible_text(ns("upload_ready")),
    # Variable selection panel ----
    shiny::conditionalPanel(
      condition = "output.upload_ready",
      ns = ns,
      # First column: safety population and treatment arms ----
      col_6(
        shiny::wellPanel(
          class = "upload",
          ## Select safety flag variable ----
          flex_row(
            row_left_side(
              shiny::selectInput(
                ns("safety_flag_variable"),
                "Safety population",
                choices = NULL
              ),
              class = "required"
            ),
            row_right_side(
              shinyWidgets::pickerInput(
                ns("safety_flag_value"),
                "Value",
                choices = "Y",
                selected = "Y",
                multiple = TRUE,
                options = picker_input_options()
              )
            )
          ),
          shiny::hr(),
          # Select treatment arms ----
          flex_row(
            row_left_side(
              shiny::selectInput(
                ns("treatment_variable"),
                "Treatment arm",
                choices = NULL
              ),
              class = "required"
            ),
            row_right_side()
          ),
          flex_row(
            ## Select Verum ----
            row_half(
              class = "required",
              shinyWidgets::pickerInput(
                ns("select_verum"),
                "Verum arm",
                choices = "Verum",
                selected = "Verum",
                multiple = TRUE,
                options = picker_input_options()
              ),
              shinyWidgets::pickerInput(
                ns("select_comparator"),
                "Comparator arm",
                choices = "Comparator",
                selected = "Comparator",
                multiple = TRUE,
                options = picker_input_options()
              )
            ),
            ## Select Comparator ----
            row_half(
              class = "required",
              shiny::textInput(
                ns("verum_name"),
                "Verum name",
                value = "Verum"
              ),
              shiny::textInput(
                ns("comparator_name"),
                "Comparator name",
                value = "Comparator"
              )
            )
          )
        )
      ),
      # Second column: variable mapping ----
      col_6(
        shiny::wellPanel(
          # AE-related variables ----
          ## Select treatment-emergent flag variable ----
          flex_row(
            row_left_side(
              class = "required",
              shiny::selectInput(
                ns("treatment_emergent_flag"),
                "Treatment-emergent AE (TEAE)",
                choices = NULL
              )
            ),
            row_right_side(
              shinyWidgets::pickerInput(
                ns("treatment_emergent_flag_value"),
                "Value",
                choices = "Y",
                selected = "Y",
                multiple = TRUE,
                options = picker_input_options()
              )
            )
          ),
          ## Select serious flag variable ----
          flex_row(
            row_left_side(
              class = "required",
              shiny::selectInput(
                ns("serious_flag_variable"),
                "Serious AE (SAE)",
                choices = NULL
              )
            ),
            row_right_side(
              shinyWidgets::pickerInput(
                ns("serious_flag_value"),
                "Value",
                choices = "Y",
                selected = "Y",
                multiple = TRUE,
                options = picker_input_options()
              )
            )
          ),
          ## Select drug-related flag variable ----
          flex_row(
            row_left_side(
              class = "required",
              shiny::selectInput(
                ns("drug_related_flag_variable"),
                "Drug-related",
                choices = NULL
              )
            ),
            row_right_side(
              shinyWidgets::pickerInput(
                ns("drug_related_flag_value"),
                "Value",
                choices = "Y",
                selected = "Y",
                multiple = TRUE,
                options = picker_input_options()
              )
            )
          ),
          ## Select AE outcome variable ----
          flex_row(
            row_left_side(
              shiny::selectInput(
                ns("ae_outcome_variable"),
                "AE outcome",
                choices = NULL
              )
            ),
            row_right_side()
          ),
          shiny::hr(),
          # Duration-related variables (for incidence rates) ----
          ## Choose duration or time start and end ----
          flex_row(
            row_left_side(
              class = "duration-mode",
              shiny::radioButtons(
                ns("duration_mode"),
                "Time at risk variables",
                choices = c(
                  "Duration" = "duration",
                  "Start and End Dates" = "start_end_date",
                  "None" = "none"
                ),
                selected = "none"
              )
            ),
            row_right_side()
          ),
          shiny::conditionalPanel(
            condition = "input.duration_mode == 'duration'",
            ns = ns,
            ## Select AE duration variable ----
            flex_row(
              row_left_side(
                shiny::selectInput(
                  ns("ae_duration_variable"),
                  "Time until AE",
                  choices = NULL
                )
              ),
              row_right_side()
            ),
            ## Select exposure duration variable ----
            flex_row(
              row_left_side(
                shiny::selectInput(
                  ns("exposure_duration_variable"),
                  "Duration of exposure",
                  choices = NULL
                )
              ),
              row_right_side()
            )
          ),
          shiny::conditionalPanel(
            condition = "input.duration_mode == 'start_end_date'",
            ns = ns,
            flex_row(
              row_left_side(
                shiny::selectInput(
                  ns("ae_start_variable"),
                  "AE start date",
                  choices = NULL
                )
              ),
              row_right_side()
            ),
            flex_row(
              row_half(
                shiny::selectInput(
                  ns("exposure_start_variable"),
                  "Start Analysis Date",
                  choices = NULL
                )
              ),
              row_half(
                shiny::selectInput(
                  ns("exposure_end_variable"),
                  "End Analysis Date",
                  choices = NULL
                )
              )
            )
          )
        )
      ),
      # Go select button ----
      shiny::actionButton(
        ns("go_select"),
        "Apply selection!",
        icon = shiny::icon("redo"),
        class = "btn-lg align-center"
      ),
      shiny::textOutput(ns("select_missing")),
      invisible_text(ns("select_ready")),
    ),
    # Feedback after selecting variables ----
    shiny::conditionalPanel(
      condition = "output.select_ready",
      ns = ns,
      class = "flex-row",
      # Buttons to go to next pages ----
      shiny::actionButton(
        ns("next_filter"),
        "Go to Filter",
        icon = shiny::icon("filter"),
        class = "btn-lg"
      ),
      shiny::actionButton(
        ns("next_double_dot"),
        "Go to Double Dot Plot",
        icon = shiny::icon("list-alt"),
        class = "btn-lg"
      ),
      shiny::actionButton(
        ns("next_heatmap"),
        "Go to Heatmap",
        icon = shiny::icon("th"),
        class = "btn-lg"
      ),
      # Show dataset info ----
      shiny::div(
        class = "flex-row",
        mod_info_ui("info_upload")
      )
    )
  )
}

#' upload module Server
#'
#' @param id Internal parameters for \code{shiny}.
#' @param r A \code{shiny::reactiveValues()} list for internal module
#' communication.
#' \describe{
#'   \item{\code{mode}}{Either "demo" (using demo data) or "sas" (using ADAM
#' files).}
#'   \item{\code{meddra_mode}}{Either "with_meddra" (using MedDRA data) or
#' "without_meddra".}
#'   \item{\code{meddra_data}}{A dataframe of filtered and prepared MedDRA
#' data.}
#'   \item{\code{smq_data}}{A dataframe of filtered and prepared MedDRA SMQ
#' data.}
#'   \item{\code{mlg_data}}{A dataframe of filtered and prepared MedDRA MLG
#' data.}
#'   \item{\code{meddra_version}}{A character with the MedDRA version used,
#' e.g., "28.1".}
#'   \item{\code{treatment_variable}}{A character. The variable name of the
#' treatment arm variable in ADSL dataset, e.g., "TR01TA".}
#'   \item{\code{verum_name}}{A character. The custom name for the verum arm.}
#'   \item{\code{comparator_name}}{A character. The custom name for the
#' comparison arm.}
#'   \item{\code{adsl_data}}{A dataframe of filtered and prepared ADSL data.}
#'   \item{\code{adae_data}}{A dataframe of filtered and prepared ADAE data.}
#'   \item{\code{adsl_variable_names}}{A character vector of variable names in
#' ADSL. Used for selecting variable filters.}
#'   \item{\code{unfiltered_data}}{A dataframe of joined ADSL-ADAE data, before
#' applying variable filters.}
#'   \item{\code{filtered_data}}{A dataframe of joined ADSL-ADAE data, after
#' applying variable filters.}
#'   \item{\code{filter_list}}{A vector describing the filters applied to ADSL
#' and ADAE data.}
#'   \item{\code{frequency_measure}}{Either "proportions" or "incidence rates".}
#'   \item{\code{effect_measure}}{Either "RR" (relative risk) or "RD" (risk
#' difference).}
#'   \item{\code{adjustment_method}}{Either "FDR" (False Discovery Rate) or
#' "DFDR" (New Double False Discovery Rate) method of p-value adjustment for
#' multiplicity.}
#'   \item{\code{stratified_by}}{A variable name, if calculations are stratified
#'  by it. "None" if the results are not stratified.}
#'   \item{\code{show_full_labels}}{An integer. Showing full or collapsed labels
#'  for double dot plot.}
#'   \item{\code{alpha}}{A character. The significance level to calculate
#' p-values and confidence intervals, e.g., "0.05".}
#'   \item{\code{results}}{A dataframe ready to be plotted.}
#'   \item{\code{go_calculate}}{An integer. Updates each time "Calculate!" is
#' pressed, and updates the calculations and plots.}
#'   \item{\code{parent_session}}{The session of the app server. Used to control
#'  the change in tabs, e.g., when pressing "Next page".}
#' }

mod_upload_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Upload, check, and prepare data files ----
    ## Upload ADAE and ADSL files ----
    adae_data_read <- shiny::reactive({
      shiny::req(input$adae_file)
      upload_file(input$adae_file)
    })
    adsl_data_read <- shiny::reactive({
      shiny::req(input$adsl_file)
      upload_file(input$adsl_file)
    })

    ## Upload MedDRA and SMQ files ----
    meddra_data_read <- shiny::reactive({
      shiny::req(input$meddra_file)
      upload_file(input$meddra_file)
    })
    smq_data_read <- shiny::reactive({
      shiny::req(input$smq_file)
      upload_file(input$smq_file)
    })

    ## Check ADAE and ADSL files (valid columns) ----
    adae_data_checked <- shiny::eventReactive(adae_data_read(), {
      shiny::req(adae_data_read())
      try_null(adae_data_read() |> check_adae_data())
    })
    adsl_data_checked <- shiny::eventReactive(adsl_data_read(), {
      shiny::req(adsl_data_read())
      try_null(adsl_data_read() |> check_adsl_data())
    })
    ## Prepare ADSL and ADAE files ----
    adae_data_prepared <- shiny::reactive({
      shiny::req(input$mode)
      if (input$mode == "demo") {
        adae <- prepare_adae_data(mode = input$mode)
      } else if (input$mode == "sas") {
        shiny::req(adae_data_checked())
        adae <- prepare_adae_data(mode = input$mode, adae = adae_data_checked())
      }
      adae
    })
    adsl_data_prepared <- shiny::reactive({
      shiny::req(input$mode)
      if (input$mode == "demo") {
        adsl <- prepare_adsl_data(mode = input$mode)
      } else if (input$mode == "sas") {
        shiny::req(adsl_data_checked())
        adsl <- prepare_adsl_data(mode = input$mode, adsl = adsl_data_checked())
      }
      adsl
    })
    ## Check MedDRA and SMQ files (valid columns) ----
    meddra_data_checked <- shiny::eventReactive(meddra_data_read(), {
      shiny::req(meddra_data_read())
      try_null(meddra_data_read() |> check_meddra_data())
    })
    smq_data_checked <- shiny::eventReactive(smq_data_read(), {
      shiny::req(smq_data_read())
      try_null(smq_data_read() |> check_smq_data())
    })
    ## Detect MedDRA versions from uploaded file ----
    meddra_numbers <- shiny::reactive({
      shiny::req(meddra_data_checked())
      numbers <- meddra_data_checked() |>
        dplyr::filter(.data$VERSION != "" & !is.na(.data$VERSION)) |>
        dplyr::distinct(.data$VERSION) |>
        dplyr::pull()
      numbers
    })
    shiny::observeEvent(meddra_numbers(), {
      shiny::req(meddra_numbers())
      shiny::updateSelectInput(
        session,
        "meddra_version",
        choices = meddra_numbers()
      )
    })
    ## Prepare MedDRA data for joining with ADAE ----
    meddra_data <- shiny::reactive({
      shiny::req(meddra_data_checked(), input$meddra_version)
      meddra_data_checked() |> prepare_meddra_data(input$meddra_version)
    })
    mlg_data <- shiny::reactive({
      shiny::req(meddra_data(), smq_data_checked(), input$meddra_version)
      smq_data_checked() |> prepare_mlg_data(input$meddra_version)
    })
    # SMQ data, only used when SMQ is selected in double-dot-plot
    smq_data <- shiny::reactive({
      shiny::req(meddra_data(), smq_data_checked(), input$meddra_version)
      smq_data_checked() |> prepare_smq_data(input$meddra_version)
    })
    ## Error checking for ADAE-ADSL or MedDRA data ----
    adae_missing <- shiny::eventReactive(
      c(input$adae_file, input$confirm_upload),
      {
        shiny::req(input$confirm_upload)
        validate_need(input$adae_file, "ADAE file is missing.")
      }
    )
    adsl_missing <- shiny::eventReactive(
      c(input$adsl_file, input$confirm_upload),
      {
        shiny::req(input$confirm_upload)
        validate_need(input$adsl_file, "ADSL file is missing.")
      }
    )
    meddra_missing <- shiny::eventReactive(
      c(input$meddra_file, input$confirm_upload),
      {
        shiny::req(input$confirm_upload)
        validate_need(input$meddra_file, "MedDRA file is missing.")
      }
    )
    smq_missing <- shiny::eventReactive(
      c(input$smq_file, input$confirm_upload),
      {
        shiny::req(input$confirm_upload)
        validate_need(input$smq_file, "MedDRA SMQ file is missing.")
      }
    )
    adae_error <- shiny::eventReactive(adae_data_read(), {
      adae_error <- catch_error_message(adae_data_read() |> check_adae_data())
      show_error(adae_error, "Please check ADAE data. ")
      "Data OK!"
    })
    adsl_error <- shiny::eventReactive(adsl_data_read(), {
      adsl_error <- catch_error_message(adsl_data_read() |> check_adsl_data())
      show_error(adsl_error, "Please check ADSL data. ")
      "Data OK!"
    })
    meddra_error <- shiny::eventReactive(meddra_data_read(), {
      validate_need(input$meddra_file, "MedDRA file is missing.")
      meddra_error <- catch_error_message(
        meddra_data_read() |> check_meddra_data()
      )
      show_error(meddra_error, "Please check MedDRA data. ")
      "Data OK!"
    })
    smq_error <- shiny::eventReactive(smq_data_read(), {
      validate_need(input$smq_file, "MedDRA SMQ file is missing.")
      smq_error <- catch_error_message(smq_data_read() |> check_smq_data())
      show_error(smq_error, "Please check MedDRA data. ")
      "Data OK!"
    })
    select_missing <- shiny::eventReactive(input$go_select, {
      shiny::req(input$go_select)
      shiny::validate(
        shiny::need(
          input$safety_flag_variable,
          message = "Please select a safety flag variable."
        ),
        shiny::need(
          input$safety_flag_value,
          message = "Please select a safety flag value"
        ),
        shiny::need(
          input$select_verum,
          message = "Please select a verum arm."
        ),
        shiny::need(
          input$select_comparator,
          message = "Please select a comparison arm."
        )
      )
    })
    output$select_missing <- shiny::renderText(select_missing())
    output$adae_missing <- shiny::renderText(adae_missing())
    output$adsl_missing <- shiny::renderText(adsl_missing())
    output$meddra_missing <- shiny::renderText(meddra_missing())
    output$smq_missing <- shiny::renderText(smq_missing())
    output$adae_check <- shiny::renderText(adae_error())
    output$adsl_check <- shiny::renderText(adsl_error())
    output$meddra_check <- shiny::renderText(meddra_error())
    output$smq_check <- shiny::renderText(smq_error())
    # Flag to show data selection panel when data upload is ready ----
    upload_is_ready <- shiny::eventReactive(input$confirm_upload, {
      NULL
      if (input$mode == "sas") {
        shiny::req(adsl_data_prepared(), adae_data_prepared())
      }
      if (input$meddra_mode == "with_meddra") {
        shiny::req(meddra_data(), smq_data(), mlg_data())
      }
      shiny::req(input$confirm_upload)
      "Upload OK! Please select required variables"
    })
    output$upload_ready <- shiny::renderText(upload_is_ready())
    # Flag to info panel when data selection is ready ----
    selection_is_ready <- shiny::eventReactive(input$go_select, {
      NULL
      shiny::req(
        input$safety_flag_variable,
        input$safety_flag_value,
        input$select_verum,
        input$select_comparator
      )
      "Selection OK! You can confirm the dataset information and move to any
      graphics page."
    })
    output$select_ready <- shiny::renderText(selection_is_ready())
    # Data selection ----
    adsl_variable_names <- shiny::eventReactive(adsl_data_prepared(), {
      shiny::req(adsl_data_prepared())
      get_variable_labels(adsl_data_prepared())
    })
    adae_variable_names <- shiny::eventReactive(adae_data_prepared(), {
      shiny::req(adae_data_prepared())
      get_variable_labels(adae_data_prepared())
    })
    ## Update the available treatment variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adsl_data_prepared(),
        var_names = adsl_variable_names(),
        name_pattern = "ACTARM|TRT01[AP]|TREATMGR",
        input_id = "treatment_variable",
        session = session
      )
    })
    ## Update the available safety flag variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adsl_data_prepared(),
        var_names = adsl_variable_names(),
        name_pattern = "SAFF",
        input_id = "safety_flag_variable",
        session = session
      )
    })
    ### Safety variable: value ----
    shiny::observeEvent(input$safety_flag_variable, {
      update_values_input(
        data_req = adsl_data_prepared(),
        input_req = input$safety_flag_variable,
        input_id = "safety_flag_value",
        session = session
      )
    })
    ## Update the TEAE available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "TRTEM",
        input_id = "treatment_emergent_flag",
        session = session
      )
    })
    ### TEAE flag value ----
    shiny::observeEvent(input$treatment_emergent_flag, {
      update_values_input(
        data_req = adae_data_prepared(),
        input_req = input$treatment_emergent_flag,
        input_id = "treatment_emergent_flag_value",
        session = session
      )
    })
    ## Update the SAE available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "AESER",
        input_id = "serious_flag_variable",
        session = session
      )
    })
    ### SAE flag value ----
    shiny::observeEvent(input$serious_flag_variable, {
      update_values_input(
        data_req = adae_data_prepared(),
        input_req = input$serious_flag_variable,
        input_id = "serious_flag_value",
        session = session
      )
    })
    ## Update the drug-related available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "AEREL",
        input_id = "drug_related_flag_variable",
        session = session
      )
    })
    ### Drug-related flag value ----
    shiny::observeEvent(input$drug_related_flag_variable, {
      update_values_input(
        data_req = adae_data_prepared(),
        input_req = input$drug_related_flag_variable,
        input_id = "drug_related_flag_value",
        session = session
      )
    })
    ## Update the Outcome available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "AEOUT",
        input_id = "ae_outcome_variable",
        session = session
      )
    })
    ## Update the AE duration available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "AAESDUR|AESDUR",
        input_id = "ae_duration_variable",
        session = session
      )
    })
    ## Update the AE start date available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adae_data_prepared(),
        var_names = adae_variable_names(),
        name_pattern = "ASTDT",
        input_id = "ae_start_variable",
        session = session
      )
    })
    ## Update the duration of exposure available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adsl_data_prepared(),
        var_names = adsl_variable_names(),
        name_pattern = "DUREXP",
        input_id = "exposure_duration_variable",
        session = session
      )
    })
    ## Update the exposure start available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adsl_data_prepared(),
        var_names = adsl_variable_names(),
        name_pattern = "TRTSDT|RFSTDT|RANDDT",
        input_id = "exposure_start_variable",
        session = session
      )
    })
    ## Update the exposure end available variables ----
    shiny::observeEvent(input$confirm_upload, {
      update_variables_input(
        data_req = adsl_data_prepared(),
        var_names = adsl_variable_names(),
        name_pattern = "TRTEDT|RFENDT|EOSDT",
        input_id = "exposure_end_variable",
        session = session
      )
    })
    ## Update Verum arm group ----
    available_treatment_arms <- shiny::eventReactive(
      input$treatment_variable,
      {
        shiny::req(input$treatment_variable)
        # Select all possible values from the selected treatment arm variable
        choices <- adsl_data_prepared() |>
          dplyr::select(tidyselect::all_of(input$treatment_variable)) |>
          dplyr::filter(.data[[input$treatment_variable]] != "") |>
          # remove formats
          haven::zap_formats() |>
          dplyr::distinct() |>
          dplyr::pull() |>
          sort()
        choices
      }
    )
    shiny::observeEvent(available_treatment_arms(), {
      shiny::req(available_treatment_arms())
      choices <- available_treatment_arms()
      shinyWidgets::updatePickerInput(
        session,
        "select_verum",
        choices = choices,
        # By default, select the first treatment arm as verum
        selected = choices[1]
      )
      if (r$mode == "demo") {
        shinyWidgets::updatePickerInput(
          session,
          "select_verum",
          selected = "Verum"
        )
      }
    })
    ## Update Comparator arm group ----
    shiny::observeEvent(c(available_treatment_arms(), input$select_verum), {
      shiny::req(
        available_treatment_arms(),
        input$select_verum
      )
      choices <- available_treatment_arms()
      choices <- choices[!choices %in% input$select_verum]
      shinyWidgets::updatePickerInput(
        session,
        "select_comparator",
        choices = choices,
        # By default, select the last available arm as comparison
        selected = rev(choices)[1]
      )
      if (r$mode == "demo") {
        shinyWidgets::updatePickerInput(
          session,
          "select_comparator",
          selected = "Comparator"
        )
      }
    })
    ## Verum and comparator custom names ----
    shiny::observeEvent(input$select_verum, {
      shiny::updateTextInput(
        session,
        "verum_name",
        value = paste(input$select_verum, collapse = ", ")
      )
    })
    shiny::observeEvent(input$select_comparator, {
      shiny::updateTextInput(
        session,
        "comparator_name",
        value = paste(input$select_comparator, collapse = " / ")
      )
    })
    ## Filter ADSL treatment arms and safety population ----
    adsl_trta <- shiny::eventReactive(input$go_select, {
      shiny::req(
        input$treatment_variable,
        input$select_verum,
        input$select_comparator,
        adsl_data_prepared(),
        input$safety_flag_variable,
        input$safety_flag_value,
        input$go_select
      )
      adsl_trta <- adsl_data_prepared() |>
        dplyr::mutate(
          trta_detector = dplyr::case_when(
            .data[[input$treatment_variable]] %in%
              input$select_comparator ~
              "Comparator",
            .data[[input$treatment_variable]] %in% input$select_verum ~ "Verum"
          ),
          trta_detector = factor(.data$trta_detector)
        ) |>
        dplyr::filter(
          .data$trta_detector %in% c("Comparator", "Verum"),
          # Filter safety population
          .data[[input$safety_flag_variable]] %in% input$safety_flag_value
        )
      adsl_trta
    })
    ## Join ADSL and ADAE datasets ----
    unfiltered_data <- shiny::reactive({
      shiny::req(adsl_trta(), adae_data_prepared())
      data <- join_adsl_adae(
        adsl = adsl_trta(),
        adae = adae_data_prepared()
      )
      data
    })
    # Join ADAE-ADSL with MedDRA datasets ----
    ## HLGT/HLT and MLG ----
    unfiltered_data_mlg <- shiny::reactive({
      shiny::req(unfiltered_data(), r$meddra_data, r$mlg_data)
      unfiltered_data() |>
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
      shiny::bindEvent(unfiltered_data())
    ## SMQ ----
    unfiltered_data_smq <- shiny::reactive({
      shiny::req(unfiltered_data(), r$smq_data)
      unfiltered_data() |>
        join_meddra_data(
          mode = "smq",
          meddra_data = r$smq_data
        )
    }) |>
      shiny::bindEvent(unfiltered_data())
    # Move to next page ----
    ## Move to filtering
    shiny::observeEvent(input$next_filter, {
      shinydashboard::updateTabItems(
        session = r$parent_session,
        inputId = "tabs",
        selected = "filter"
      )
    })
    ## Move directly to double-dot-plot
    shiny::observeEvent(input$next_double_dot, {
      shinydashboard::updateTabItems(
        session = r$parent_session,
        inputId = "tabs",
        selected = "graph"
      )
    })
    ## Move directly to heatmap
    shiny::observeEvent(input$next_heatmap, {
      shinydashboard::updateTabItems(
        session = r$parent_session,
        inputId = "tabs",
        selected = "heatmap"
      )
    })
    # Pass objects to reactiveValues "r" ----
    shiny::observe({
      r$mode <- input$mode
    })
    shiny::observe({
      r$meddra_mode <- input$meddra_mode
    })
    shiny::observe({
      r$meddra_data <- meddra_data()
    })
    shiny::observe({
      r$smq_data <- smq_data()
    })
    shiny::observe({
      r$mlg_data <- mlg_data()
    })
    shiny::observe({
      r$meddra_version <- input$meddra_version
    })
    shiny::observe({
      r$treatment_variable <- input$treatment_variable
    })
    shiny::observe({
      r$treatment_emergent_flag_variable <- input$treatment_emergent_flag
    })
    shiny::observe({
      r$treatment_emergent_flag_value <- input$treatment_emergent_flag_value
    })
    shiny::observe({
      r$serious_flag_variable <- input$serious_flag_variable
    })
    shiny::observe({
      r$serious_flag_value <- input$serious_flag_value
    })
    shiny::observe({
      r$drug_related_flag_variable <- input$drug_related_flag_variable
    })
    shiny::observe({
      r$drug_related_flag_value <- input$drug_related_flag_value
    })
    shiny::observe({
      r$ae_outcome_variable <- input$ae_outcome_variable
    })
    shiny::observe({
      r$duration_mode <- input$duration_mode
    })
    shiny::observe({
      r$exposure_duration_variable <- input$exposure_duration_variable
    })
    shiny::observe({
      r$ae_duration_variable <- input$ae_duration_variable
    })
    shiny::observe({
      r$exposure_start_variable <- input$exposure_start_variable
    })
    shiny::observe({
      r$exposure_end_variable <- input$exposure_end_variable
    })
    shiny::observe({
      r$ae_start_variable <- input$ae_start_variable
    })
    shiny::observe({
      r$verum_name <- input$verum_name
    })
    shiny::observe({
      r$comparator_name <- input$comparator_name
    })
    shiny::observe({
      r$adsl_data <- adsl_trta()
    })
    shiny::observe({
      r$adae_data <- adae_data_prepared()
    })
    shiny::observe({
      r$adsl_variable_names <- adsl_variable_names()
    })
    shiny::observe({
      r$adae_variable_names <- adae_variable_names()
    })
    # We store the datasets as "filtered data", even if they're not filtered,
    # because this will be the object that will be plotted later on. If we
    # later add filters using the Filter tab, filtered_data will be updated
    shiny::observe({
      r$unfiltered_data <- unfiltered_data()
      r$filtered_data <- unfiltered_data()
    })
    shiny::observe({
      r$filtered_data_mlg <- unfiltered_data_mlg()
    })
    shiny::observe({
      r$filtered_data_smq <- unfiltered_data_smq()
    })
    shiny::observe({
      r$go_select <- input$go_select
    })
  })
}
