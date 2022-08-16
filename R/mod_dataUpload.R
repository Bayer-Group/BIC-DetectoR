#' dataUpload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @export
#' @importFrom shiny NS tagList
mod_dataUpload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinydashboard::sidebarMenu(
      id = ns("my_sidebarmenu"),
      shiny::tags$head(
        shiny::tags$style(
          ".inactiveLink {
            pointer-events: none;
            cursor: default;
          }"
        )
      ),
      br(),
      shiny::conditionalPanel(condition = "output.flag == false && output.flag2 == false",
        shiny::br(),
        shiny::tags$div(
          shiny::HTML(paste(
            shiny::tags$span(
              style = "font-size:100%",
              "Please ", shiny::tags$span(style = "color:#EE2C2C", "upload"),
              " your Adverse Event and Subject data ", br(), " files and select the corresponding ", br(),
              shiny::tags$span(style = "color:#EE2C2C", "MedDRA Version"),
              "first. ", br(), "Use the panel below.
              If you want to run the App ", br(), "without MedDRA please select 'Run without MedDRA',", br(), "
              The Options are then not available.",
              sep = ""
            )
          )
        )
      )
    ),
    shiny::uiOutput(ns("uploadData")),
    shiny::conditionalPanel(condition = "output.flag == true",
      shiny::wellPanel(
        style = paste0("background-color: ", col_midgrey, "; border-color: white; border: 0px; border-radius: 0px;"),
        shinyWidgets::prettyToggle(
          inputId = "showSelectTreatment",
          label_off = HTML("<span style='color: white; font-size: 15px;'> Selection (required) </span>"),
          label_on = HTML("<span style = 'color: white;font-size: 15px; '> Selection (required) </span>"),
          value = TRUE,
          outline = TRUE,
          status_on = "default",
          status_off = "default",
          plain = TRUE,
          icon_off = icon("pills"),
          icon_on = icon("pills")
        ),
        conditionalPanel(
          condition = "input.showSelectTreatment",
          shiny::uiOutput(ns("measureOutput")),
          shiny::uiOutput(ns("selectTreatment")),
          shiny::uiOutput(ns("selectVerum")),
          shiny::uiOutput(ns("selectComparator")),
          shiny::actionButton(ns("apply2"),
            "Apply Selection!",
            icon = icon("redo"),
            style = "color: #fff; background-color: #61a337; border-color: #fff"
          )
        )
      )
    ),  
    shinydashboard::menuItem("View dataset", tabName = "safety", icon = icon("table")),
    shinydashboard::menuItem("Double Dot Plot", tabName = "graph", icon = icon("list-alt")),
    shinydashboard::menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
    shinydashboard::menuItem("About DetectoR", tabName = "helptext", icon = icon("info-circle")),
    shinydashboard::menuItem("Data Manual", tabName = "datamanual", icon = icon("folder-open"))
    ),
    shiny::conditionalPanel(
      condition = "output.flag == true",
      shiny::wellPanel(
        style = paste0("background-color: ", col_midgrey, "; border-color: white; border: 0px; border-radius: 0px;"),
        shinyWidgets::prettyToggle(
          inputId = "showSelectFilter",
          label_off = HTML("<span style = 'color: white; font-size: 15px;'> Select Filter (optional) </span>"),
          label_on = HTML("<span style = 'color: white; font-size: 15px;'> Select Filter (optional) </span>"),
          value = FALSE,
          outline = TRUE,
          status_on = "default",
          status_off = "default",
          plain = TRUE,
          icon_off = icon("filter"),
          icon_on = icon("filter")
        ),
        conditionalPanel(
          condition = "input.showSelectFilter",
          shiny::uiOutput(ns("pickerinput_adae")),
          shiny::uiOutput(ns("pickerinput_adsl")),
          shiny::fluidRow(
            shiny::column(
              4,
              shiny::actionButton(
                inputId = ns("insertBtn"),
                label = "Add",
                icon = icon("plus")
              )
            ),
            shiny::column(
              4,
              shiny::actionButton(
                inputId = ns("removeBtn"),
                label = "Delete",
                icon = icon("minus")
              )
            )
          ),
          shiny::tags$div(id = "placeholder"),
          shiny::conditionalPanel(
            condition = "output.condition_filter == true",
            shiny::actionButton(
              inputId = ns("apply"),
              label = "Apply Filter Selection!",
              icon = icon("redo"),
              style = "color: #fff; background-color: #61a337; border-color: #fff"
            )
          )
        )
      ),
      mod_report_ui("report_ui_1")
    ),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text = jsResetCode, functions = c("reset_detector")),
    shiny::actionButton(
      inputId = ns("go_reload"),
      label = "Reload App",
      icon = icon("sync"),
      style = "color: #fff; background-color: #EE2C2C; border-color: #fff"
    ),
    br(),
    HTML(paste0("This version (from ", "2022-07-29",") of", br())),
    HTML(paste0(img(
      src = "AppSign_white_BAG_DetectoR_220x76mm_RGB.png",
      height = 35,
      align = "center"
    ))),
    HTML(paste0(br(), "was developed under ", R.Version()$version.string,".", br())),
    shinyjs::extendShinyjs(text = jscode, functions = c("disableTab", "enableTab")),
    shinyjs::inlineCSS(css)
  )
}

#' dataUpload Server Function
#' @export
#' @noRd
mod_dataUpload_server <- function(input, output, session) {
  ns <- session$ns

  #TRTA_Detector <- NULL 
  
  col_midgrey <- "#4d4d4d"
  col_lightgrey <- "#595959"
  col_darkgrey <- "#353535"
  
  
  shiny::observeEvent(input$go_reload, {
    shinyjs::js$reset()
  })

  adae_data <- shiny::reactive({
    shiny::req(input$meddraVersion)
    shiny::req(input$mode)
    select_adae_data(meddra_Version = input$meddraVersion, mode = input$mode, adae_file = input$adae_file)
  })

  output$pickerinput_adae <- shiny::renderUI({
    shiny::req(adae_data(),adsl_data_TRTA())
    
    adsl_data <- adsl_data_TRTA()
    
    adsl_data_variables_tmp <- purrr::map(
      adsl_data,
      function(x) attr(x, "label", exact = TRUE)
    )
    adsl_data_variables = names(adsl_data_variables_tmp)
    names(adsl_data_variables) = paste0(
      names(adsl_data_variables_tmp),
      ifelse(
        as.character(adsl_data_variables_tmp) == "NULL",
        "",
        paste0(" - ", as.character(adsl_data_variables_tmp))
      )
    )
    
    adae_data <- adae_data()
    
    adae_data_variables_tmp <- purrr::map(
      adae_data,
      function(x) attr(x, "label", exact = TRUE)
    )
    adae_data_variables = names(adae_data_variables_tmp)
    names(adae_data_variables) <- paste0(
      names(adae_data_variables_tmp),
      ifelse(
        as.character(adae_data_variables_tmp) == "NULL", 
        "", 
        paste0(" - ", as.character(adae_data_variables_tmp))
      )
    )
    choices <- adae_data_variables[!(adae_data_variables  %in% adsl_data_variables)]
    colors <- rep("color: white; background: #424242;", length(choices))
    
    shinyWidgets::pickerInput(
      inputId = ns('pickerinput_adae'),
      label = 'Select filter variable(s) for adae data set',
      choices = choices, 
      selected = NULL,
      multiple = TRUE,
      choicesOpt = list(style = colors),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search` = TRUE,
        `header` = 'Select multiple items',
        `none-selected-text` = 'No selection!'
      )
    )
  })

  #### ... 3. pickerinput_adsl ####
  output$pickerinput_adsl <- shiny::renderUI({
    shiny::req(adsl_data_TRTA())
    
    adsl_data <- adsl_data_TRTA()
    
    adsl_data_variables_tmp <- purrr::map(
      adsl_data,
      function(x) attr(x, "label", exact = TRUE)
    )
    adsl_data_variables = names(adsl_data_variables_tmp)
    names(adsl_data_variables) = paste0(
      names(adsl_data_variables_tmp),
      ifelse(
        as.character(adsl_data_variables_tmp) == "NULL",
        "",
        paste0(" - ", as.character(adsl_data_variables_tmp))
      )
    )
    choices <- adsl_data_variables 
    #remove TRTA_Detector for filter selection
    choices <- choices[choices != "TRTA_DetectoR"]
    choices <- choices[choices != input$selectTreatment]
    colors <- rep("color: white; background: #424242;", length(choices))
    shinyWidgets::pickerInput(
      inputId = ns('pickerinput_adsl'),
      label = 'Select filter variable(s) for adsl data set', 
      choices = choices, 
      selected = NULL,
      multiple = TRUE,
      choicesOpt = list(style = colors),
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = 'count > 0',
        `count-selected-text` = '{0} selected (of {1})',
        `live-search` = TRUE,
        `header` = 'Select multiple items',
        `none-selected-text` = 'No selection!'
      )
    )
  })

  #### ... 4. insertBtn ####
  shiny::observeEvent(c(adsl_data_TRTA()), {
    shinyjs::useShinyjs()
    if (input$insertBtn == 0) {
      shinyjs::click(ns("insertBtn"))
    }
  })

  adsl_data_TRTA <- shiny::reactive({
    shiny::req(input$pickerinput_Verum, input$pickerinput_Comparator, adsl_data())
    adsl <- adsl_data()

    adsl <- adsl %>%
      dplyr::mutate(TRTA_DetectoR = dplyr::case_when(
        !!rlang::sym(selectTreatment$val) %in% input$pickerinput_Comparator ~ "Comparator",
        !!rlang::sym(selectTreatment$val) %in% input$pickerinput_Verum ~ "Verum"
      )) %>%
      dplyr::filter(TRTA_DetectoR == "Comparator" | TRTA_DetectoR == "Verum")

    adsl
  })

  inserted_adae <- c()

  id_adae_nr <- c()
  id_adae_nr2 <- c()

  id_adae_m <- shiny::reactiveValues()
  id_adae_m$myList <- list()
  id_adae_m$myList2 <- list()
  inserted_adae_list <- shiny::reactive({
    list()
  })

  inserted_adsl <- c()

  id_adsl_nr <- c()
  id_adsl_nr2 <- c()

  id_adsl_m <- shiny::reactiveValues()
  id_adsl_m$myList <- list()
  id_adsl_m$myList2 <- list()
  inserted_adsl_list <- shiny::reactive({
    list()
  })

  shiny::observeEvent(c(adsl_data_TRTA()), {
    shinyjs::useShinyjs()
    if (input$insertBtn == 0) {
      shinyjs::click(ns("insertBtn"))
    }
  })

  condition_filter <- shiny::reactiveValues(val = FALSE)
  output$condition_filter <- shiny::reactive(condition_filter$val)
  
  # observe the 'Insert' Button click:
  shiny::observeEvent(c(input$insertBtn), {
    shiny::req(adae_data(),adsl_data_TRTA())
    
    adae_data <- adae_data()
    adsl_data <- adsl_data_TRTA()
    
    ins_adae <- inserted_adae_list()
    id_adae_nr <<- c()
    id_adae_nr2 <<- c()
    ins_adsl <- inserted_adsl_list()
    id_adsl_nr <<- c()
    id_adsl_nr2 <<- c()
    
    if (length(inserted_adae) > 0) {
      for (i in 1:length(inserted_adae)) {
        shiny::removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted_adae[i])
        )
      }}
    if (length(inserted_adsl) > 0) {
      for (i in 1:length(inserted_adsl)) {
        shiny::removeUI(
          ## pass in appropriate div id
          selector = paste0('#', inserted_adsl[i])
        )
      }
    }
    inserted_adae <<- c()
    inserted_adsl <<- c()
    
    btn <- input$insertBtn
    
    pickerinput_adae <- input$pickerinput_adae
    pickerinput_adsl <- input$pickerinput_adsl
    if (length(pickerinput_adae) > 0) {
      for (i in 1: length(pickerinput_adae)) {
        id <- paste0(pickerinput_adae[i], btn)
        shiny::insertUI(
          selector = '#placeholder',
          
          ui = shiny::tags$div(
            if (!is.numeric(adae_data %>%
                           dplyr::pull(pickerinput_adae[i]))) {
              shinyWidgets::pickerInput(
                inputId = ns(id),
                label = paste0(pickerinput_adae[i]),
                choices = adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  unique,
                selected = adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  unique,
                multiple = TRUE,
                choicesOpt = list(style = rep("color: white; background: #424242;",
                  length(adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  unique))),
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format` = 'count > 0',
                  `count-selected-text` = '{0} selected (of {1})',
                  `live-search` = TRUE,
                  `header` = 'Select multiple items',
                  `none-selected-text` = 'All dropped!'
                )
              )
            } else if (
              is.numeric(
                adae_data %>%
                  dplyr::pull(pickerinput_adae[i])
              ) && !is.integer(
                adae_data %>%
                  dplyr::pull(pickerinput_adae[i])
                )
            ) {
              shiny::sliderInput(
                inputId = ns(id),
                label = paste0(pickerinput_adae[i]),
                value = c(
                  adae_data %>%
                    dplyr::pull(pickerinput_adae[i]) %>%
                    base::min(na.rm = TRUE), adae_data %>%
                    dplyr::pull(pickerinput_adae[i]) %>%
                    base::max(na.rm = TRUE)
                ),
                min = adae_data %>%
                 dplyr::pull(pickerinput_adae[i]) %>%
                 base::min(na.rm = TRUE),
                max = adae_data %>%
                 dplyr::pull(pickerinput_adae[i]) %>%
                 base::max(na.rm = TRUE)
              )
            } else if (is.numeric(adae_data %>%
                                 dplyr::pull(pickerinput_adae[i])) && is.integer(adae_data %>%
                                                                                 dplyr::pull(pickerinput_adae[i]))) {
              shiny::sliderInput(
                inputId = ns(id), 
                label = paste0(pickerinput_adae[i]),
                value = c(adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  base::min(na.rm = TRUE),adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  base::max(na.rm = TRUE)
                ),
                min = adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  base::min(na.rm = TRUE),
                max = adae_data %>%
                  dplyr::pull(pickerinput_adae[i]) %>%
                  base::max(na.rm = TRUE),
                step = 1,
                sep = "",
                ticks = FALSE
              )
            },
            id = id
          )
        )
        inserted_adae <<- c(id, inserted_adae)
        ins_adae[[pickerinput_adae[i]]]  <- adae_data %>%
          dplyr::pull(pickerinput_adae[i])
        id_adae_nr2 <<- c(id_adae_nr2, pickerinput_adae[[i]])
        id_adae_nr <<- c(id_adae_nr,id)
      }
    }
    if (length(pickerinput_adsl) > 0) {  
      for(i in 1: length(pickerinput_adsl)){
        id <- paste0(pickerinput_adsl[i], btn)
        shiny::insertUI(
          selector = '#placeholder',
          ui = shiny::tags$div(
            if(!is.numeric(adsl_data %>%
                           dplyr::pull(pickerinput_adsl[i]))){
              shinyWidgets::pickerInput(
                inputId = ns(id),
                label = paste0(pickerinput_adsl[i]),
                choices = adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i]) %>%
                  unique,
                selected = adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i]) %>%
                  unique,
                multiple = TRUE,
                choicesOpt = list(style = rep("color: white; background: #424242;",
                  length(adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i]) %>%
                  unique))),
                options = list(
                  `actions-box` = TRUE,
                  `selected-text-format`='count > 0',
                  `count-selected-text` = '{0} selected (of {1})',
                  `live-search` = TRUE,
                  `header` = 'Select multiple items',
                  `none-selected-text` = 'All dropped!'
                )
              )
            } else if (is.numeric(adsl_data %>%
                                dplyr::pull(pickerinput_adsl[i])) && !is.integer(adsl_data %>%
                                                                                 dplyr::pull(pickerinput_adsl[i]))) {
              shiny::sliderInput(
                inputId = ns(id), 
                label = paste0(pickerinput_adsl[i]),
                value = c(
                  (adsl_data %>%
                    dplyr::pull(pickerinput_adsl[i])) %>%
                    base::min( na.rm = TRUE), (adsl_data %>%
                    dplyr::pull(pickerinput_adsl[i])) %>%
                    base::max( na.rm = TRUE)
                ),
                min = (adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i])) %>%
                  base::min( na.rm = TRUE),
                max = (adsl_data %>%
                dplyr::pull(pickerinput_adsl[i])) %>%
                base::max( na.rm = TRUE))
            } else if (is.numeric(adsl_data %>%
                                dplyr::pull(pickerinput_adsl[i])) && is.integer(adsl_data %>%
                                                                                dplyr::pull(pickerinput_adsl[i]))){
              shiny::sliderInput(
                inputId = ns(id),
                label = paste0(pickerinput_adsl[i]),
                value = c((adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i])) %>%
                  base::min( na.rm = TRUE), base::max((adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i])), na.rm = TRUE)),
                min = (adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i])) %>%
                  base::min( na.rm = TRUE),
                max = (adsl_data %>%
                  dplyr::pull(pickerinput_adsl[i])) %>%
                  base::max(na.rm = TRUE),
                step = 1,
                sep = "", 
                ticks = FALSE
              )
            },
            id = id
          )
        )
        inserted_adsl <<- c(id, inserted_adsl)
        ins_adsl[[pickerinput_adsl[i]]]  <- adsl_data %>%
          dplyr::pull(pickerinput_adsl[i])
        id_adsl_nr2 <<- c(id_adsl_nr2,pickerinput_adsl[[i]])
        id_adsl_nr <<- c(id_adsl_nr,id)
      }
    }
    
    if (length(id_adae_nr) > 0 | length(id_adsl_nr) > 0) {
      condition_filter$val <- TRUE
    } else {
      condition_filter$val <- FALSE
    }
    id_adae_m$myList2 <- id_adae_nr2
    id_adae_m$myList <- id_adae_nr
    id_adsl_m$myList2 <- id_adsl_nr2
    id_adsl_m$myList <- id_adsl_nr
  })
  
  output$measureOutput <- shiny::renderUI({
    shiny::req(adsl_data())
    shiny::req(input$mode)
    if(length(intersect(unique(c(colnames(adae_data()),
                                 colnames(adsl_data()))),
                        c("AAESDURN", "DUREXP"))) == 2){
      shinyWidgets::pickerInput(ns("measure"), "Select Measure",
                                choices = c("proportions", "incidence rates"), 
                                selected = "proportions", multiple = FALSE,
                                options = list(
                                  `actions-box` = TRUE,
                                  `selected-text-format` = "count > 0",
                                  `count-selected-text` = "{0} selected (of {1})",
                                  `live-search` = TRUE,
                                  `header` = "Select multiple items",
                                  `none-selected-text` = "No selection!"
                                )
      )
    } else {
      shinyWidgets::pickerInput(ns("measure"), "Select Measure", choices = "proportions", selected = "proportions")
    }
  })

  selectTreatment <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$selectTreatment, {
    selectTreatment$val <- input$selectTreatment
  })

  output$selectTreatment <- shiny::renderUI({
    shiny::req(adsl_data())
    shiny::req(input$mode)
    if (input$mode != "demo") {
      tmp <- adsl_data()

      adsl_variables_tmp <- purrr::map(tmp, function(x) attr(x, "label", exact = TRUE))
      adsl_variables <- names(adsl_variables_tmp)
      names(adsl_variables) <- paste0(names(adsl_variables_tmp), ifelse(as.character(adsl_variables_tmp) == "NULL", "", paste0(" - ", as.character(adsl_variables_tmp))))
      choices <- adsl_variables
      choices <- c(choices[stringr::str_detect(choices, "TRT01") | stringr::str_detect(choices, "ARM")], choices[!(stringr::str_detect(choices, "ARM") | stringr::str_detect(choices, "TRT01"))])
      shinyWidgets::pickerInput(ns("selectTreatment"), 
                                "Select Treatment",
        choices = choices,
        selected = NULL,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 0",
          `count-selected-text` = "{0} selected (of {1})",
          `live-search` = TRUE,
          `header` = "Select multiple items",
          `none-selected-text` = "No selection!"
        )
      )
    } else {
      shinyWidgets::pickerInput(ns("selectTreatment"), "Select Treatment", choices = "TREATMGR", selected = "TREATMGR")
    }
  })

  output$selectVerum <- shiny::renderUI({
    shiny::req(adsl_data(), input$selectTreatment)
    if (input$mode != "demo") {
      adsl <- adsl_data()
      choices <- adsl %>%
        dplyr::select(selectTreatment$val) %>%
        dplyr::filter(input$selectTreatment != "") %>%
        dplyr::distinct() %>%
        dplyr::pull() %>%
        sort()
      shinyWidgets::pickerInput(ns("pickerinput_Verum"), "Select Verum",
        choices = choices, selected = NULL, multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 0",
          `count-selected-text` = "{0} selected (of {1})",
          `live-search` = TRUE,
          `header` = "Select multiple items",
          `none-selected-text` = "No selection!"
        )
      )
    } else {
      shinyWidgets::pickerInput(ns("pickerinput_Verum"), "Select Verum", choices = "VERUM", selected = "VERUM")
    }
  })

  output$selectComparator <- shiny::renderUI({
    shiny::req(adsl_data(), input$selectTreatment)
    if (input$mode != "demo") {
      adsl <- adsl_data()
      choices <- adsl %>%
        dplyr::select(selectTreatment$val) %>%
        dplyr::filter(input$selectTreatment != "") %>%
        dplyr::distinct() %>%
        dplyr::pull() %>%
        sort()
      choices <- choices[!choices %in% input$pickerinput_Verum]
      
      shinyWidgets::pickerInput(ns("pickerinput_Comparator"),
        "Select Comparator",
        choices = choices, 
        selected = NULL, 
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `selected-text-format` = "count > 0",
          `count-selected-text` = "{0} selected (of {1})",
          `live-search` = TRUE,
          `header` = "Select multiple items",
          `none-selected-text` = "No selection!"
        )
      )
    } else {
      shinyWidgets::pickerInput(ns("pickerinput_Comparator"), "Select Comparator", choices = "COMPARATOR", selected = "COMPARATOR")
    }
  })

  output$uploadData <- shiny::renderUI({
    meddra_numbers_list <- as.list(c("", "NoMeddra", meddra_numbers))
    names(meddra_numbers_list) <- c("", "Run without MedDRA", paste0("Meddra Version ", meddra_numbers))
    shiny::wellPanel(
      style = paste0("background-color: ", col_midgrey, "; border-color: white; border: 0px; border-radius: 0px;"),
      shinyWidgets::prettyToggle(
        inputId = "sh_Upload",
        label_off = HTML("<span style='color: white; font-size: 15px;'> Upload Data (required) </span>"),
        label_on = HTML("<span style = 'color: white;font-size: 15px;'> Upload Data (required) </span>"),
        value = TRUE,
        outline = TRUE,
        status_on = "default",
        status_off = "default",
        plain = TRUE,
        icon_off = icon("upload"),
        icon_on = icon("upload")
      ),
      conditionalPanel(condition = "input.sh_Upload",
        shiny::radioButtons(
          inputId = ns("mode"),
          label = "Input mode:",
          choices = c(
            "SAS file (from Disc)" = "sas",
            "Demo data" = "demo"
          )
        ),
        shiny::conditionalPanel(condition = paste0("input['", ns("mode"), "\'] == \'sas\'"),
          shiny::fileInput(ns("adae_file"),
            "AE data",
            multiple = FALSE,
            accept = NULL,
            width = NULL
          ),
          shiny::fileInput(ns("adsl_file"),
            "Subject  data  (SAFFN variable must be present)",
            multiple = FALSE,
            accept = NULL,
            width = NULL
          )
        ),
        shiny::conditionalPanel(
          condition = paste0("input['", ns("mode"), "\'] == \'portin\'"),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("ok"), "\'] != \'null\'", " && ", "input['", ns("ok"), "\'] != \''"),
            shiny::selectizeInput("projects",
              "Portin projects",
              "",
              options = list(
                placeholder = "Projects loading",
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          shiny::conditionalPanel(
            condition = paste0("input['", ns("projects"), "\'] != \'null\'", " && ", "input['", ns("projects"), "\'] != \''"),
            shiny::selectizeInput(ns("sel_studies"),
              "Studies",
              choices = "",
              multiple = TRUE,
              options = list(
                placeholder = "...",
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            actionButton(ns("ok_studies"), "Select studies")
          )
        ),
        shiny::selectInput(ns("meddraVersion"),
          label = HTML(paste0("Select a MedDRA Version <span style='color: #EE2C2C; font-size: 16px;'> (required) </span>")),
          selected = NULL,
          choices = c(meddra_numbers_list)
        ),
        # shiny::conditionalPanel(condition = paste0("input['", ns("mode"), "\'] !== \'server\'"),
          shiny::conditionalPanel(condition = "input.switch_adts == true",
            shiny::fileInput(
              inputId = ns("adts_file"),
              label = "Add adts for MedDRA Information",
              multiple = FALSE,
              accept = NULL,
              width = NULL
            )
          ),
          shiny::conditionalPanel(condition = paste0("input['", ns("mode"), "\'] !== \'demo\'"),
            shinyWidgets::materialSwitch(
              inputId = "switch_adts",
              label = HTML("<span style = 'color: white;'> ...or add adts data for MedDRA Information </span>"),
              status = "success"
            )
          )
        # )
      )
    )
  })

  output$studySelect <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("studies"),
      label = "Select a study",
      choices = studies
    )
  })

  output$cont_treatment <- shiny::renderUI({
    shiny::req(col_treatment$val)
    list(
      shiny::tags$style(
        paste(
          ".box.box-warning>.box-header {
            color:", col_treatment$val, ";
            background:", col_treatment$val, "
            }
            .box.box-warning{
            background:", col_treatment$val, ";
            border-top-color:", col_treatment$val, ";
          }
          ", sep = ""
        )
      )
    )
  })
  
  shiny::observeEvent(input$adts_file, {
    adts <- haven::read_sas(input$adts_file$datapath)
    if (all(c("TSPARM", "TSVAL") %in% names(adts))) {
      selected <- adts %>% 
        dplyr::select(TSPARM,TSVAL) %>% 
        dplyr::filter(TSPARM == "MedDRA Version") %>% 
        dplyr::pull(TSVAL) %>% 
        as.character()
      
      if (selected %in% meddra_numbers) {
        shiny::updateSelectInput(
          session,
          inputId = "meddraVersion",
          selected = selected
        )
      } else {
        print("No MedDRA Version found! Dataset requires a row with condition TSPARM == 'MedDRA Version'")
      }
    } else {
      print("No MedDRA Version found! Dataset requires Variable TSPARM and TSVAL")
    }
  })

  adae_yavin <- shiny::reactiveValues(
    val = NULL
  )
  adsl_yavin <- shiny::reactiveValues(
    val = NULL
  )
  adts_yavin <- shiny::reactiveValues(
    val = NULL
  )

  error_message_adae <- reactiveValues(val = "")
  error_message_flag_adae <- shiny::reactiveValues(val = FALSE)

  shiny::observeEvent(input$mode, {
    shiny::req(input$mode)
    if (all(meddra_numbers == "")) {
      if (input$mode == "demo") {
        shiny::updateSelectInput(
          session,
          "meddraVersion",
          choices = c("",
            'Run without MedDRA'='NoMeddra'
          ),
          selected = 'NoMeddra'
        )
      }
    } else {
      if (input$mode == "demo" & is.null(input$meddraVersion)) {
        shiny::updateSelectInput(
          session,
          "meddraVersion",
          choices = c("",
            'Run without MedDRA'='NoMeddra',
            'Meddra Version 20.1' = '20.1'
          ),
          selected = NULL
        )
      } else if (input$mode == "demo" &
                 !is.null(input$meddraVersion) & 
                  input$meddraVersion %in% c("",'Run without MedDRA'='NoMeddra','Meddra Version 20.1' = '20.1')
        ) {
        shiny::updateSelectInput(
          session,
          inputId = "meddraVersion",
          choices = c("",'Run without MedDRA'='NoMeddra',
          'Meddra Version 20.1' = '20.1'),
          selected = input$meddraVersion
        )
      } else if (input$mode == "demo" & 
                 !is.null(input$meddraVersion) &
                 !(input$meddraVersion %in% c("",'Run without MedDRA'='NoMeddra','Meddra Version 20.1' = '20.1'))) {
        shiny::updateSelectInput(
          session,
          inputId = "meddraVersion",
          choices = c("", 'Run without MedDRA' = 'NoMeddra',
          'Meddra Version 20.1' = '20.1'),
          selected = 'NoMeddra'
        )
      } else {
        
        meddra_numbers_list <- as.list(c("", "NoMeddra", meddra_numbers))
        names(meddra_numbers_list) <- c("", "Run without MedDRA", paste0("Meddra Version ", meddra_numbers))
        
        shiny::updateSelectInput(session,
          "meddraVersion",
          choices = meddra_numbers_list,
          selected = NULL
        )
      }
    }
  })

  flag_treatment <- shiny::reactive({
    shiny::req(input$apply2)
    1
  })
  
  # Portin: add code read ADSL
  adsl_data <- shiny::reactive({
    shiny::req(input$mode)
      select_adsl_data(mode = input$mode, adsl_file = input$adsl_file)
  })

  # end Portin add code
  adsl_data_TRTA <- shiny::reactive({
    shiny::req(input$pickerinput_Verum, input$pickerinput_Comparator, adsl_data())
    adsl <- adsl_data()

    adsl <- adsl %>%
      dplyr::mutate(TRTA_DetectoR = dplyr::case_when(
        !!rlang::sym(selectTreatment$val) %in% input$pickerinput_Comparator ~ "Comparator",
        !!rlang::sym(selectTreatment$val) %in% input$pickerinput_Verum ~ "Verum"
      )) %>%
      dplyr::filter(TRTA_DetectoR == "Comparator" | TRTA_DetectoR == "Verum")
    adsl
  })

  tmp_adae <- shiny::reactive({
    shiny::req(adae_data())
    adae_data <- adae_data()
    data <- adae_data
    if (length(id_adae_m$myList) != 0) {
      names <- id_adae_m$myList2
      vars <- id_adae_m$myList
      if (length(id_adae_m$myList) && !is.null(id_adae_m$myList2)) {
        data_filt <- data
        for (i in 1:length(id_adae_m$myList)) {
          if(adae_data %>%
             dplyr::pull(id_adae_m$myList2[i]) %>%
             is.numeric()) {
            if(!is.null(input[[id_adae_m$myList[[i]]]]))
              data_filt <- data_filt[data_filt %>%
                                       dplyr::pull(id_adae_m$myList2[i]) %>%
                                       dplyr::between(input[[id_adae_m$myList[[i]]]][1],input[[id_adae_m$myList[[i]]]][2]),]
          }else{
            data_filt <- data_filt %>%
              dplyr::filter(!! rlang::sym(id_adae_m$myList2[i]) %in% c(input[[id_adae_m$myList[i]]]))
          }
        }
      }
    } else {data_filt <- data}
    data_filt
  })

  tmp_adsl <- shiny::reactive({
    shiny::req(adsl_data_TRTA())
    adsl_data <- adsl_data_TRTA()
    data <- adsl_data
    if (id_adsl_m$myList %>%
      length() != 0) {
      names <- id_adsl_m$myList2
      vars <- id_adsl_m$myList
      if (length(id_adsl_m$myList) && !is.null(id_adsl_m$myList2)) {
        data_filt <- data
        for (i in 1:length(id_adsl_m$myList)) {
          if (adsl_data %>%
            dplyr::pull(id_adsl_m$myList2[i]) %>%
            is.numeric()) {
            if (!is.null(input[[id_adsl_m$myList[[i]]]])) {
              data_filt <- data_filt[data_filt %>%
                dplyr::pull(id_adsl_m$myList2[i]) %>%
                dplyr::between(input[[id_adsl_m$myList[[i]]]][1], input[[id_adsl_m$myList[[i]]]][2]), ]
            }
          } else {
            data_filt <- data_filt %>%
              dplyr::filter(!!rlang::sym(id_adsl_m$myList2[i]) %in% c(input[[id_adsl_m$myList[i]]]))
          }
        }
      }
    } else {
      data_filt <- data
    }
    data_filt
  })

  # Create a Condition on the Apply/Actionbutton
  adae_filtered <- shiny::eventReactive(c(input$apply, input$apply2), {
    data <- tmp_adae()
    data
  })
  # Create a Condition on the Apply/Actionbutton
  adsl_filtered <- shiny::eventReactive(c(input$apply, input$apply2), {
    data <- tmp_adsl()
    data
  })
  # Reset initial values if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    id_adae_m$myList <- list()
    id_adae_m$myList2 <- list()
  })

  # Delete UI Elements if Remove Button is clicked
  shiny::observeEvent(input$removeBtn, {
    for (i in 1:length(inserted_adae)) {
      removeUI(selector = paste0("#", inserted_adae[i]))
    }
    for (i in 1:length(inserted_adsl)) {
      removeUI(selector = paste0("#", inserted_adsl[i]))
    }
  })

   filter_list <- shiny::reactiveValues(
    val = NULL
  )
  
 shiny::observeEvent(input$apply, {
    tmp_list <- list(ADAE = c(),ADSL = c())
    if (!is.null(input$pickerinput_adae)) {
      for (i in 1:length(input$pickerinput_adae)) {
        if (!is.null(paste0(input$pickerinput_adae[[i]],input$insertBtn))) {
          tmp_list$ADAE[[i]] <- paste0(
            "Dataset adae is filtered by ",
            input$pickerinput_adae[[i]],
            ": ",
            paste(input[[paste0(input$pickerinput_adae[[i]], input$insertBtn)]], collapse = ", ")
          )
        }
      }
    }
    if (!is.null(input$pickerinput_adsl)) {
      for (i in 1:length(input$pickerinput_adsl)) {
        if (!is.null(paste0(input$pickerinput_adsl[[i]],input$insertBtn))) {
          tmp_list$ADSL[[i]] <- paste0(
            "Dataset adsl is filtered by ",
            input$pickerinput_adsl[[i]],
            ": ",
            paste(input[[paste0(input$pickerinput_adsl[[i]], input$insertBtn)]], collapse = ifelse(is.numeric(input[[paste0(input$pickerinput_adsl[[i]], input$insertBtn)]])," - ", ", "))
          )
        }
      }
    }
    filter_list$val <- paste(unlist(tmp_list))
  })
 
 ## display options for incidence rates if the appropriate variables are found
 # selectMeasure <- shiny::reactiveValues(val = "proportions")
 # 
 # shiny::observeEvent(input$measure, {
 #   shiny::req(adsl_data())
 #   shiny::req(input$mode)
 #   print(paste0("measure: ", input$measure))
 #   print(colnames(adae_data()))
 #   print(colnames(adsl_data()))
 #   print(intersect(unique(c(colnames(adae_data()),
 #                            colnames(adsl_data()))),
 #                   c("AAESDURN", "DUREXP")))
 #   selectMeasure$val <- input$measure
 # })
 # observe({
 #   if(length(intersect(unique(c(colnames(adae_data()),
 #                                colnames(adsl_data()))),
 #                       c("AAESDURN", "DUREXP"))) == 2){
 #     output$measure <- renderUI({
 #       shiny::req(adsl_data())
 #       shiny::req(input$mode)
 #       shinyWidgets::prettyRadioButtons(ns("measure"),
 #                                        label = HTML("<span style = 'color: white;'> Choose Measure </span>"),
 #                                        choices = c("proportions", "incidence rates"),
 #                                        selected = selectMeasure$val,
 #                                        fill = TRUE, status = "info"
 #       )
 #     })
 #   } else {
 #     output$measure <- renderUI({
 #       shiny::req(adsl_data())
 #       shiny::req(input$mode)
 #       shinyWidgets::prettyRadioButtons(ns("measure"),
 #                                        label = HTML("<span style = 'color: white;'> Choose Measure </span>"),
 #                                        choices = c("proportions"),
 #                                        selected = selectMeasure$val,
 #                                        fill = TRUE, status = "info"
 #       )
 #     })
 #   }
 # })
 

 
  return(list(
    my_sidebarmenu = shiny::reactive({
      input$my_sidebarmenu
    }),
    adae_data = adae_data,
    adsl_data = adsl_data,
    adae_filtered = adae_filtered,
    adsl_filtered = adsl_filtered,
    adsl_data_TRTA = adsl_data_TRTA,
    meddraVersion = shiny::reactive({
      input$meddraVersion
    }),
    measure = shiny::reactive({
      # selectMeasure$val
      input$measure
    }),
    selectTreatment = shiny::reactive({
      input$selectTreatment
    }),
    pickerinput_Verum = shiny::reactive({
      input$pickerinput_Verum
    }),
    pickerinput_Comparator = shiny::reactive({
      input$pickerinput_Comparator
    }),
    go_reload = shiny::reactive({
      input$go_reload
    }),
    apply = shiny::reactive({
      input$apply
    }),
    apply2 = shiny::reactive({
      input$apply2
    }),
    condition_filter = shiny::reactive({
      condition_filter$val
    }),
    mode = shiny::reactive({
      input$mode
    }),
    adae_file = shiny::reactive({
      input$adae_file
    }),
    adsl_file = shiny::reactive({
      input$adsl_file
    }),
    pickerinput_adae = shiny::reactive({
      input$pickerinput_adae
    }),
    pickerinput_adsl = shiny::reactive({
      input$pickerinput_adsl
    }),
    error_message_flag_adae = shiny::reactive({error_message_flag_adae$val}),
    error_message_adae = shiny::reactive({error_message_adae$val}),
    filter_list = filter_list
  ))
}

## To be copied in the UI
# mod_dataUpload_ui("dataUpload_ui_1")

## To be copied in the server
# callModule(mod_dataUpload_server, "dataUpload_ui_1")
