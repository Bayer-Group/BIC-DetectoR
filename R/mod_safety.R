#' safety UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @export
#' @importFrom shiny NS tagList
mod_safety_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::wellPanel(
      style = paste0("background-color: ", col_midgrey, "; border-color: ", col_lightgrey, "; border: 5px; border-radius: 5px;"),
      shinyWidgets::prettyToggle(
        inputId = ns("showPanel1"),
        label_off = HTML("<span style='color: white; font-size: 16px;'> Select main options  </span>"),
        label_on = HTML("<span style = 'color: white; font-size: 16px;'> Select main options  </span>"),
        value = TRUE,
        outline = TRUE,
        status_on = "default",
        status_off = "default",
        plain = TRUE,
        icon_off = icon("cogs"),
        icon_on = icon("cogs")
      ),
      conditionalPanel(
      condition = paste0('input[\'', ns('showPanel1'), "\']"),
      shiny::fluidRow(
        shiny::column(
          1,
          shiny::actionButton(ns("go_tab1"),
            "Apply!",
            icon = icon("redo"),
            style = "color: #fff; background-color: #61a337; border-color: #fff"
          )
        ),
        shiny::column(
          2,
          shiny::uiOutput(ns("SI_var"))
        ),
        shiny::column(
          2,
          shiny::uiOutput(ns("SI_disp"))
        ),
        shiny::column(
          2,
          shiny::uiOutput(ns("SI_adjust"))
        ),
        shiny::column(
          1,
          shinyWidgets::materialSwitch(
            inputId = ns("switch"),
            label = HTML("<span style = 'color: white;'> Advanced Settings </span>"),
            status = "success",
            value = FALSE
          )
        )
      ),
      shiny::conditionalPanel(
      condition = "output.advanced == true",
        shiny::fluidRow(
          shiny::column(
            1,
            shiny::uiOutput(ns("SI_strat1"))
          ),
          shiny::column(
            2,
            shiny::uiOutput(ns("SI_fisher_alternative"))
          ),
          shiny::column(
            4,
            shiny::uiOutput(ns("SI_filter1"))
          ),
          shiny::column(
            1,
            shiny::uiOutput(ns("SI_alpha"))
          )
        )
      )
    )
  ),
  conditionalPanel(condition = "output.flag == false && output.flag == false && output.error_message_flag == false",
    h2("Welcome to DetectoR"),
    shiny::uiOutput(ns("myImage")),
    h3("The DetectoR R Shiny app provides a handy platform allowing for early identification of signals and an ongoing monitoring of safety along the medical product development phase and lifecycle.")
  ),
  conditionalPanel(
  condition = "output.flag == true && output.flag2 == false",
    shiny::tags$div(
      shiny::HTML(
        paste(
          shiny::tags$span(
            style = "font-size:150%",
            "Please select a ", shiny::tags$span(style = "color:#EE2C2C", "treatment, verum and comparator"),
            " on the left side and submit with the ", shiny::tags$span(style = "color:#61a337", "apply-button"),"."
          )
        )
      )
    )
  ),
  shiny::conditionalPanel(condition = paste0('output.error_message_flag == true && output[\'', ns('mode_flag'), "\'] == 'server'"),
    uiOutput(ns("error_message_adae"))
  ),
  shiny::conditionalPanel(condition = "output.flag == true && output.flag2 == true",
      shiny::column(6, 
        shiny::wellPanel(style = paste0("background-color: ", col_midgrey, "; border-color: ", col_lightgrey, "; border: 5px; border-radius: 5px;"),
          shinyWidgets::prettyToggle(
            inputId = ns('showhelptextPanel1_1'),
            label_off = HTML("<span style='color: white; font-size: 16px;'> Dataset Information </span>"),
            label_on = HTML("<span style = 'color: white; font-size: 16px;'> Dataset Information </span>"),
            value = TRUE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("info-circle"),
            icon_on = icon ("info-circle")
          ),
          shiny::conditionalPanel(condition = paste0('input[\'', ns('showhelptextPanel1_1'), "\']"),
            shiny::uiOutput(
              ns('tot_info1_1')
            )
          )
        )
      ),
      shiny::column(6, 
        shiny::wellPanel(style = paste0("background-color: ", col_midgrey, "; border-color: ", col_lightgrey, "; border: 5px; border-radius: 5px;"),
          shinyWidgets::prettyToggle(
            inputId = ns('showhelptextPanel1_3'),
            label_off = HTML("<span style='color: white; font-size: 16px;'> Study Site Information </span>"),
            label_on = HTML("<span style = 'color: white; font-size: 16px;'> Study Site Information </span>"),
            value = FALSE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("university"),
            icon_on = icon ("university")
          ),
          shiny::conditionalPanel(condition = paste0('input[\'', ns('showhelptextPanel1_3'), "\']"),
              shiny::uiOutput(ns('tot_info1_3'))
          )
        )
      ),
      shiny::column(12,
        shiny::wellPanel(style = paste0("background: ", col_midgrey ,"; border-width: 0px;"),
          shiny::fluidRow(
             shiny::uiOutput(ns('adsl1'))
          )
        )
      )
    )
  )
}

#' Reactive values for mod_safety_ui Function module
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @export
mod_safety_ui_vars <- function(input, output, session) {
  
  return(
    list(
      go_tab1 = shiny::reactive({
        input$go_tab1}),
      SI_var1 = shiny::reactive({
        input$SI_var1}),
      switch = shiny::reactive({
        input$switch})
    )
  )
}


#' safety Server Function
#'
#'@export
#' @noRd
mod_safety_server <- function(input, output, session, mod_dataUpload_server_vars, 
                              go_tab2, SI_var, SI_disp, SI_order_by, SI_adjust, 
                              alpha, SI_filter, SI_strat, SI_fisher_alternative, calcDDP, min.event) {
  ns <- session$ns
  
  output$error_message_adae <- shiny::renderUI({
    if (mod_dataUpload_server_vars$error_message_flag_adae()) {
      HTML(paste0("<span style='color: #EE2C2C; font-size: 150%;'> ", mod_dataUpload_server_vars$error_message_adae() ," </span>"))
    }
  })
  
  output$mode_flag <- shiny::reactive({mod_dataUpload_server_vars$mode()})
  shiny::outputOptions(output, "mode_flag", suspendWhenHidden = FALSE)
  
  
  shiny::observeEvent(c(input$SI_var1), {
    SI_var$val <- input$SI_var1
  })

  output$SI_var <- shiny::renderUI({
    shiny::req(mod_dataUpload_server_vars$meddraVersion())
    if (mod_dataUpload_server_vars$meddraVersion() != "NoMeddra") {
      shiny::selectInput(ns("SI_var1"),
        label = HTML("<span style = 'color: white;'> Safety variable </span>"),
        choices = list(
          "System Organ Class" = "AEBODSYS",
          "Preferred term" = "AEDECOD",
          "Medical labeling grouping" = "MLG_label"
        ),
        selected = "AEBODSYS"
      )
    } else {
      shiny::selectInput(ns("SI_var1"),
        label = HTML("<span style = 'color: white;'> Safety variable </span>"),
        choices = list(
          "System Organ Class" = "AEBODSYS",
          "Preferred term" = "AEDECOD"
        ),
        selected = "AEBODSYS"
      )
    }
  })
  shiny::observeEvent(input$SI_disp1, {
    SI_disp$val <- input$SI_disp1
  })

  output$SI_disp <- shiny::renderUI({
    shiny::selectInput(ns("SI_disp1"),
      label = HTML("<span style = 'color: white;'> Effect </span>"),
      choices = list(
        "Relative Risk" = "RR",
        "Risk Difference" = "RD"
      ),
      selected = SI_disp$val
    )
  })
  shiny::observeEvent(input$go_tab1, {
    SI_adjust$val <- input$SI_adjust1
  })

  output$SI_adjust <- shiny::renderUI({
    shiny::selectInput(ns("SI_adjust1"),
      label = HTML("<span style = 'color: white;'> p-value adjustment </span>"),
      choices = list(
        "False Discovery Rate" = "FDR",
        "New Double False Discovery Rate" = "DFDR"
      ),
      selected = SI_adjust$val
    )
  })

  output$SI_strat1 <- shiny::renderUI({
  
    shiny::req(mod_dataUpload_server_vars$adsl_data_TRTA())

    adsl_data <- mod_dataUpload_server_vars$adsl_data_TRTA()

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
    choices <- as.character(c("None", adsl_data_variables))
    
    shiny::selectInput(
      inputId = ns("SI_strat1"), 
      label = HTML("<span style = 'color: white;'> Stratify by </span>"),
      selected = SI_strat$val,
      choices = choices
    )
  })
  
  observeEvent(input$SI_strat1,{
    if (input$SI_strat1 != "None") {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "SI_fisher_alternative1",
        choices = list("Two sided" = "two.sided"),
        selected = "two.sided"
      )
    } else {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "SI_fisher_alternative1",
        choices = list(
          "Two sided" = "two.sided",
          "One sided" = "less"
        ),
        selected = SI_fisher_alternative$val
      )
    }
  })
    
  shiny::observeEvent(input$SI_fisher_alternative1, {
    SI_fisher_alternative$val <- input$SI_fisher_alternative1
  })

  output$SI_fisher_alternative1 <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
        inputId = ns("SI_fisher_alternative"),
        label = HTML("<span style = 'color: white;'> Alternative for Fisher Test </span>"),
        choices = list(
          "Two sided" = "two.sided",
          "One sided" = "less"
        ),
        selected = SI_fisher_alternative$val,
        fill = TRUE, status = "info"
      )
  })

  output$SI_filter1 <- shiny::renderUI({
    shiny::req(min.event())
    minE <- min.event()
    minEv <- paste0("Use the minimum number of events in Verum (n = ", minE, ")")
    choices <- list("nomethod", "onepercent", "min")
    names(choices) <- c("No Method", "Overall incidence >= 1%", minEv)
    shinyWidgets::prettyRadioButtons(ns("SI_filter1"),
      label = HTML("<span style = 'color: white;'> AE Filtering </span>"),
      choices = choices,
      selected = SI_filter$val,
      fill = TRUE, status = "info"
    )
  })

  shiny::observeEvent(input$SI_filter1, {
    SI_filter$val <- input$SI_filter1
  })

  output$SI_alpha <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(ns("SI_alpha1"),
      label = HTML("<span style = 'color: white;'> Significance level (alpha) </span>"),
      choices = c("0.01", "0.05", "0.1"),
      selected = alpha$val,
      fill = TRUE,
      status = "info"
    )
  })

  shiny::observeEvent(c(input$SI_alpha1), {
    alpha$val <- input$SI_alpha1
  })

  output$myImage <- shiny::renderUI({
    list(shiny::HTML("<img src = 'AppIcon_BAG_DetectoR_210x210mm_RGB.png' alt = 'Graphic cannot be displayed' width = '400' height = '400'>"))
  })

  output$tot_info1 <- renderUI({
    shiny::req(mod_dataUpload_server_vars$adsl_filtered(), mod_dataUpload_server_vars$adae_filtered())
    HTML(paste0(
      "<b style = 'color: white;'>", "Total number ", "of subjects (ADSL): ", dim(mod_dataUpload_server_vars$adsl_filtered())[1], "</b>",
      "<b style = 'color: 1e90ff;'>", "&nbsp; &nbsp; &nbsp; Verum (N = ", dim(mod_dataUpload_server_vars$adsl_filtered() %>%
        dplyr::filter(TRTA_DetectoR == "Verum"))[1], ")", "</b>",
      "<b style = 'color: #EE2C2C#;'>", "&nbsp; &nbsp; &nbsp; Comparator (N = ", dim(mod_dataUpload_server_vars$adsl_filtered() %>%
        dplyr::filter(TRTA_DetectoR == "Comparator"))[1], ")", "</b>",
      "<b style = 'color: white;'>", "&nbsp; &nbsp; &nbsp; Total AE lines (ADAE): ", dim(mod_dataUpload_server_vars$adae_filtered())[1], "</b>"
    ))
  })

  output$adsl1 <- shiny::renderUI({
    DT::dataTableOutput(ns("adsl"))
  })
  
  output$adsl <- DT::renderDataTable({
    if(!is.null(calcDDP())){
      tmp <- calcDDP()
      tmp <- tmp %>%
        dplyr::select(-c(col.p, col.p_adj, p.value.lab, p_adj.value)) %>%
        dplyr::mutate_if(is.numeric, round, 4)
      
      variable <- shiny::isolate(SI_var$val)
      RR_text <- paste0("RR ", isolate(100 * (1 - as.numeric(alpha$val))), "% CI")
      RD_text <- paste0("RD ", isolate(100 * (1 - as.numeric(alpha$val))), "% CI")
      if (tmp %>% tibble::has_name("p")) {
        tmp$p <- ifelse(round(tmp$p, 4) == 0, "<0.0001", format(round(tmp$p, 4), scientific = FALSE))
      }
      if (tmp %>% tibble::has_name("p_adj")) {
        tmp$p_adj <- ifelse(round(tmp$p_adj, 4) == 0, "<0.0001", format(round(tmp$p_adj, 4), scientific = FALSE))
      }
      if (tmp %>% tibble::has_name("DFDR")) {
        tmp$DFDR <- ifelse(round(tmp$DFDR, 4) == 0, "<0.0001", format(round(tmp$DFDR, 4), scientific = FALSE))
      }
      
      if (isolate(SI_adjust$val) == "DFDR") {
        if (!is.null(variable) & variable == "AEBODSYS") {
          renameList <- rlang::quos(SOC = AEBODSYS, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = rr.missing_label)
          varList <- rlang::quos(SOC, "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        } else if (!is.null(variable) & variable == "AEDECOD") {
          renameList <- rlang::quos(SOC = AEBODSYS, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = DFDR)
          varList <- rlang::quos(PT, "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        } else if (!is.null(variable) & variable == "MLG_label") {
          renameList <- rlang::quos("MLG/PT" = PT, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = DFDR)
          varList <- rlang::quos("MLG/PT", "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        }
      } else {
        if (!is.null(variable) & variable == "AEBODSYS") {
          renameList <- rlang::quos(SOC = AEBODSYS, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = p_adj)
          varList <- rlang::quos(SOC, "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        } else if (!is.null(variable) & variable == "AEDECOD") {
          renameList <- rlang::quos(SOC = AEBODSYS, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = p_adj)
          varList <- rlang::quos(PT, "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        } else if (!is.null(variable) & variable == "MLG_label") {
          renameList <- rlang::quos("MLG/PT" = PT, Events = count, "Treatment (N)" = TRTA_DetectoR, "Risk difference" = rd, "Relative risk" = rr, Incidence = prop, "unadjusted p-value" = p, "adjusted p-value" = p_adj)
          varList <- rlang::quos("MLG/PT", "Treatment (N)", Events, Incidence, "Risk difference", !!rlang::sym(RD_text), "Relative risk", !!rlang::sym(RR_text), "adjusted p-value", "unadjusted p-value")
        }
      }
      
      tmp <- tmp %>%
        dplyr::mutate(
          !!rlang::sym(RD_text) := glue::glue("[{format(rd.lcl, scientific = FALSE)}, {format(rd.ucl, scientific = FALSE)}]"),
          !!rlang::sym(RR_text) := glue::glue("[{format(rr.lcl, scientific = FALSE)}, {format(rr.ucl, scientific = FALSE)}]")
        ) %>%
        dplyr::rename(!!!renameList) %>%
        dplyr::select(!!!varList)
      
      
      DT::datatable(tmp,
                    extensions = "Buttons",
                    escape = FALSE,
                    options = list(
                      scrollX = TRUE,
                      initComplete = DT::JS(
                        "function(settings, json) {",
                        paste0('"$(this.api().table().header()).css({\'color\': \'white\'});"'),
                        "}"
                      ),
                      dom = "Brtip", 
                      buttons = c("copy", "print", "pageLength", I("colvis")), 
                      lengthMenu = list(c(10, 50, 100, -1), c("10", "50", "100", "All")), 
                      pageLength = 10,
                      rowCallback = DT::JS("function(row, data) {\n 
        // Bold cells for those >= 5 in the first column\n
        if (parseFloat(data[1]) >= 15.0)\n 
        $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
        }")
                    ),
                    class = "cell-border stripe", 
                    rownames = FALSE,
                    caption = "Table of calculated Results",
                    filter = "top"
      ) %>%
        DT::formatStyle(1, 
                        target = "row", 
                        color = "white", 
                        backgroundColor = col_lightgrey)
    } else {
      shinyalert("Nothing to display...", type = "error")
      DT::datatable(data.frame())
    }
  })
  
  output$tot_info1_1 <- shiny::renderUI({
    shiny::req(tot_info_text1_1())
    HTML(tot_info_text1_1())
  })

  tot_info_text1_1 <- shiny::reactive({
    shiny::req(mod_dataUpload_server_vars$adsl_data_TRTA(), mod_dataUpload_server_vars$adae_data(),
      mod_dataUpload_server_vars$adsl_filtered(), mod_dataUpload_server_vars$adae_filtered())

    adsl_data_TRTA <- mod_dataUpload_server_vars$adsl_data_TRTA()
    adae_data <- mod_dataUpload_server_vars$adae_data()
    adsl_filtered <- mod_dataUpload_server_vars$adsl_filtered()
    adae_filtered <- mod_dataUpload_server_vars$adae_filtered()
    
    comb_data <- dplyr::left_join(
      adsl_data_TRTA, 
      adae_data,
      by = c("STUDYID","USUBJID")
    )
    comb_data_filtered <- dplyr::left_join(
      adsl_filtered, 
      adae_filtered,
      by = c("STUDYID","USUBJID")
    )
    
   if (dim(adsl_data_TRTA)[1] > dim(adsl_filtered)[1]) {
      is_adsl_filtered <- TRUE
    } else {
      is_adsl_filtered <- FALSE
    }
    
    if (dim(comb_data)[1] > dim(comb_data_filtered)[1]) {
      is_filtered <- TRUE
    } else {
      is_filtered <- FALSE
    }

    total <- comb_data %>% 
      dplyr::select(USUBJID) %>% 
      tidyr::drop_na() %>% 
      dplyr::pull() %>% 
      unique() %>% 
      length()

    total_verum <- comb_data %>% 
      dplyr::select(USUBJID, TRTA_DetectoR) %>% 
      tidyr::drop_na() %>% 
      dplyr::filter(TRTA_DetectoR == "Verum") %>% 
      dplyr::pull(USUBJID) %>% 
      unique() %>% 
      length()
    
    total_comparator <- comb_data %>% 
      dplyr::select(USUBJID, TRTA_DetectoR) %>% 
      tidyr::drop_na() %>% 
      dplyr::filter(TRTA_DetectoR == "Comparator") %>% 
      dplyr::pull(USUBJID) %>% 
      unique() %>% 
      length()
    
    ae_total <- comb_data %>%
      dplyr::select(USUBJID,AEDECOD,AEBODSYS) %>%
      tidyr::drop_na() %>% 
      dplyr::select(USUBJID) %>%
      dplyr::pull()%>% 
      unique() %>% 
      length()
    
    ae_total_verum <- comb_data %>%
      dplyr::select(USUBJID,AEDECOD,AEBODSYS, TRTA_DetectoR) %>%
      tidyr::drop_na() %>% 
      dplyr::filter(TRTA_DetectoR == "Verum") %>% 
      dplyr::select(USUBJID) %>%
      dplyr::pull(USUBJID) %>% 
      unique() %>% 
      length()
    
    ae_total_comparator <- comb_data %>%
      dplyr::select(USUBJID,AEDECOD,AEBODSYS, TRTA_DetectoR) %>%
      tidyr::drop_na() %>% 
      dplyr::filter(TRTA_DetectoR == "Comparator") %>% 
      dplyr::select(USUBJID) %>%
      dplyr::pull(USUBJID) %>% 
      unique() %>% 
      length()
    
    if (is_filtered) {
      total_filtered <- comb_data_filtered %>% 
        dplyr::select(USUBJID) %>% 
        tidyr::drop_na() %>% 
        dplyr::pull()%>% 
        unique() %>% 
        length()

      total_verum_filtered <- comb_data_filtered %>% 
        dplyr::select(USUBJID, TRTA_DetectoR) %>% 
        tidyr::drop_na() %>% 
        dplyr::filter(TRTA_DetectoR == "Verum") %>% 
        dplyr::pull(USUBJID) %>% 
        unique() %>% 
        length()
    
      total_comparator_filtered <- comb_data_filtered %>% 
        dplyr::select(USUBJID, TRTA_DetectoR) %>% 
        tidyr::drop_na() %>% 
        dplyr::filter(TRTA_DetectoR == "Comparator") %>% 
        dplyr::pull(USUBJID) %>% 
        unique() %>% 
        length()
      
      ae_total_filtered <- comb_data_filtered %>%
        dplyr::select(USUBJID,AEDECOD,AEBODSYS) %>%
        tidyr::drop_na() %>% 
        dplyr::select(USUBJID) %>%
        dplyr::pull()%>% 
        unique() %>% 
        length()
      
      ae_total_verum_filtered <- comb_data_filtered %>%
        dplyr::select(USUBJID,AEDECOD,AEBODSYS, TRTA_DetectoR) %>%
        tidyr::drop_na() %>% 
        dplyr::filter(TRTA_DetectoR == "Verum") %>% 
        dplyr::select(USUBJID) %>%
        dplyr::pull(USUBJID) %>% 
        unique() %>% 
        length()
      
      ae_total_comparator_filtered <- comb_data_filtered %>%
        dplyr::select(USUBJID,AEDECOD,AEBODSYS, TRTA_DetectoR) %>%
        tidyr::drop_na() %>% 
        dplyr::filter(TRTA_DetectoR == "Comparator") %>% 
        dplyr::select(USUBJID) %>%
        dplyr::pull(USUBJID) %>% 
        unique() %>% 
        length()
    }
    
    paste0(
      "Total number of subjects (Total/Verum/Comparator) : ", total,"/",
      "<span style = 'color: #1e90ff;'>",total_verum ,"</span>","/",
      "<span style = 'color: #EE2C2C;'>",total_comparator,"</span>" ,
      "<br>",
      ifelse(is_adsl_filtered,
        paste0("Selected number of subjects in adsl (filtered) (Total/Verum/Comparator) : ", total_filtered,"/",
       "<span style = 'color: #1e90ff;'>", total_verum_filtered ,"</span>","/",
       "<span style = 'color: #EE2C2C;'>", total_comparator_filtered,"</span>" ,
       "<br>"),""
      ),"<br>",
      "Total AE lines (ADAE): ", dim(adae_data)[1], ifelse(is_filtered, paste0(" (filtered: ",dim(adae_filtered[adae_filtered$USUBJID %in% comb_data_filtered$USUBJID,])[1],")"), ""),
      "<br>","<br>",
      "Number of subjects with AEs (Total/Verum/Comparator) : ", ae_total,"/",
      "<span style = 'color: #1e90ff;'>",ae_total_verum ,"</span>","/",
      "<span style = 'color: #EE2C2C;'>",ae_total_comparator,"</span>" ,
      "<br>",  
      ifelse(is_filtered,
          paste0(
          "Number of subjects (filtered) with AEs (Total/Verum/Comparator) : ", ae_total_filtered, "/",
          "<span style = 'color: #EE2C2C;'>", ae_total_verum_filtered ,"</span>", "/",
          "<span style = 'color: #1e90ff;'>", ae_total_comparator_filtered ,"</span>"
          ),""
        )
      )
  })
  
  output$tot_info1_3 <- shiny::renderUI({
    shiny::req(tot_info_text1_3())
    HTML(tot_info_text1_3())
  })

  tot_info_text1_3 <- shiny::reactive({
    number_studies <- length(unique(mod_dataUpload_server_vars$adsl_filtered()$STUDYID))
      list_number_subject_in_studies <- mod_dataUpload_server_vars$adsl_filtered() %>%
        dplyr::select(STUDYID, USUBJID) %>%
        unique() %>% 
        dplyr::group_by(STUDYID) %>%
        dplyr::summarise(n = dplyr::n(), .groups = 'drop') %>% 
        dplyr::mutate(new = paste0(STUDYID, " (n=",n, ")"))
      
      paste0(
        "Number of studies: ", number_studies, 
        ifelse(number_studies > 1, paste(" with STUDYID (number of subjects): " , paste(list_number_subject_in_studies$new, collapse = ", ")),"")
      )
  })
}

## To be copied in the UI
# mod_safety_ui("safety_ui_1")

## To be copied in the server
# callModule(mod_safety_server, "safety_ui_1")
