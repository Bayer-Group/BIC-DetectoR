#' heatmap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_heatmap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      style = paste0(
        "background-color: ",
        col_midgrey,
        "; border-color: ",
        col_lightgrey,
        "; border: 5px; border-radius: 5px;"
      ),
      shinyWidgets::prettyToggle(
        inputId = ns("showPanel3"),
        label_off = HTML("<span style='color: white;font-size: 16px;'> Select main options </span>"),
        label_on = HTML("<span style = 'color: white;font-size: 16px;'> Select main options </span>"),
        value = TRUE,
        outline = TRUE,
        status_on = "default",
        status_off = "default",
        plain = TRUE,
        icon_off = icon("cogs"),
        icon_on = icon("cogs")
      ),
      conditionalPanel(
        condition = paste0('input[\'', ns('showPanel3'), "\']"),
        shiny::fluidRow(
          shiny::column(
            1,
            shiny::actionButton(ns("go_tab3"),
              "Apply!",
              icon = icon("redo"),
              style = "color: #fff; background-color: #61a337; border-color: #fff"
            )
          ),
          shiny::column(
            3,
            shiny::uiOutput(ns("SI_var3"))
          ),

          shiny::column(2,
            shiny::selectInput(
              inputId = ns("heatmap_Selection"),
              label = HTML("<span style = 'color: white;'> New Selection </span>"),
              choices = list(
                "Relative Risk" = "RR",
                "Risk Difference" = "RD",
                "False Discovery Rate" = "FDR",
                "New Double False Discovery Rate" ="DFDR"
              ),
              selected = "RR"
            )
          ),
          shiny::column(
            1,
            shinyWidgets::materialSwitch(
              inputId = ns("switch3"),
              label = HTML("<span style = 'color: white;'> Advanced Settings </span>"),
              status = "success",
              value = FALSE
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "output.advanced3 == true",
          shiny::fluidRow(
            shiny::column(
              1,
              shiny::uiOutput(ns("SI_strat3"))
            ),
            shiny::column(
              4,
              shiny::uiOutput(ns("SI_filter3"))
            ),
            shiny::column(
              1,
              shiny::uiOutput(ns("SI_alpha3"))
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "output.flag == false && output.flag2 == false",
      h2("Welcome to DetectoR"),
      shiny::uiOutput(ns("myImage3")),
      h3("The DetectoR R Shiny app provides a handy platform allowing for early identification of signals and an ongoing monitoring of safety along the medical product development phase and lifecycle.")
    ),
    shiny::conditionalPanel(condition = "output.flag == true && output.flag2 == false",
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
    conditionalPanel(
    condition = "output.flag == true  && output.flag2 == false && input.mode == 'sas'",
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
    shiny::conditionalPanel(condition = "output.flag == true && output.flag2 == true",
      shiny::fluidRow(
        shiny::conditionalPanel(condition = "output.heat_flag == false",
          br(),
          shiny::tags$div(
            HTML(
              paste(
                shiny::tags$span(style = "font-size:150%",
                  shiny::tags$span(style = "color:white", "Please choose your settings and then use the "),
                  shiny::tags$span(style = "color: #61a337", "Apply Button"),
                  shiny::tags$span(style = "color:white", "in the Panel at the top."), sep = ""
                )
                
              )
            )
          )
        ),
        shiny::conditionalPanel(condition = "output.heat_flag == true",
          shiny::wellPanel(style = paste0("background: ", col_darkgrey,"; border-width: 0px;"),
           shiny::uiOutput(ns('tree1'))
          ),
          shiny::uiOutput(ns('legendPlot')),
          shiny::uiOutput(ns('arrowsPlot')),
          shiny::helpText(
            HTML('<p style = "color:white"> Click any cell to zoom in, and treemap header to zoom out.</p>')
          )
        )
      )
    )
  )
}

#' Reactive values for mod_heatmap_ui Function module
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return list with following components
#' \describe{
#'   \item{go_tab3}{go_tab3}
#' }
#' @export
mod_heatmap_ui_vars <- function(input, output, session) {
  
  return(
    list(
      go_tab3 = shiny::reactive({
        input$go_tab3}),
      switch3 = shiny::reactive({
        input$switch3})
    )
  )
}

#' heatmap Server Function
#'
#' @noRd
mod_heatmap_server <- function(input, output, session, 
                               mod_dataUpload_server_vars, SI_var, SI_strat, min.event, N_1, N_2) {
  ns <- session$ns

  output$error_message_adae <- shiny::renderUI({
    if (mod_dataUpload_server_vars$error_message_flag_adae()) {
      HTML(paste0("<span style='color: #EE2C2C; font-size: 150%;'> ", mod_dataUpload_server_vars$error_message_adae() ," </span>"))
    }
  })
  
  output$mode_flag <- shiny::reactive({mod_dataUpload_server_vars$mode()})
  shiny::outputOptions(output, "mode_flag", suspendWhenHidden = FALSE)
  
  
  heatmapPrep <- shiny::eventReactive(input$go_tab3, {
    shiny::req(mod_dataUpload_server_vars$adsl_filtered(), mod_dataUpload_server_vars$adae_filtered(),
      input$heatmap_Selection)
    #assign reactive objects
    data_adsl <- mod_dataUpload_server_vars$adsl_filtered()
    data_adae <- mod_dataUpload_server_vars$adae_filtered()
    if (dim(data_adsl)[1] > 0 & dim(data_adae)[1] > 0) { 
      if(input$heatmap_Selection == "RR") {
        SI_disp3 <- "RR"
        Heatmap_coloring <- "effect"
        SI_adjust3 <- "FDR"
      } else if (input$heatmap_Selection == "RD") {
        SI_disp3 <- "RD"
        Heatmap_coloring <- "effect"
        SI_adjust3 <- "FDR"
      } else if (input$heatmap_Selection == "FDR") {
        SI_disp3 <- "RR"
        Heatmap_coloring <- "p-value"
        SI_adjust3 <- "FDR"
      } else if (input$heatmap_Selection == "DFDR") {
        SI_disp3 <- "RR"
        Heatmap_coloring <- "p-value"
        SI_adjust3 <- "DFDR"
      }
      
      variable1 <- strsplit(req(input$SI_var3),"_H")[[1]][1]
      display1 <- SI_disp3
      adjustment <- SI_adjust3
      
      
      study_strat <- SI_strat3$val
      alph <- as.numeric(alpha3$val)
      filt <- SI_filter_heat$val
      measure <- shiny::isolate(mod_dataUpload_server_vars$measure())

      tmp <- calcDoubleDotPlot2(
        data_adsl = data_adsl,
        data_adae = data_adae,
        variable = variable1,
        display = display1,
        adjustment = adjustment,
        order_by = "effect",
        study_strat = study_strat,
        seed = 2006,
        heatmap = TRUE,
        alpha = alph,
        filter = filt,
        measure = measure
      )
      
      tmp <- tmp[complete.cases(tmp),]

    
      if (input$SI_var3 == "AEDECOD") {
        if (Heatmap_coloring == "p-value" & SI_adjust3 == "DFDR") {
          tmp <- tmp %>%
            dplyr::select(!!rlang::sym(variable1),count) %>%
            dplyr::group_by(!!rlang::sym(variable1)) %>%
            dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
            dplyr::right_join(tmp %>%
              dplyr::select(
                !!rlang::sym(variable1), prob1, prob2,
                DFDR, rr, rd, rr.lcl, rr.ucl, rd.lcl,
                rd.ucl, p, p_adj, AEBODSYS, bign, TRTA_DetectoR, count
              ) %>%
              dplyr::distinct(), by = variable1
            ) 
        } else {
          tmp <- tmp %>%
            dplyr::select(!!rlang::sym(variable1), count) %>%
            dplyr::group_by(!!rlang::sym(variable1)) %>%
            dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
            dplyr::right_join(tmp %>%
              dplyr::select(
                !!rlang::sym(variable1), prob1, prob2,
                rr, rd, rr.lcl, rr.ucl, rd.lcl, rd.ucl,
                p, p_adj, AEBODSYS, bign, TRTA_DetectoR, count
              ) %>%
              dplyr::distinct(), by = variable1
            )
        }
      } else if (input$SI_var3 == "AEDECOD_HLT") {
          if (Heatmap_coloring == "p-value" & SI_adjust3 == "DFDR") {
          tmp <- tmp %>%
            dplyr::select(!!rlang::sym(variable1),count) %>%
            dplyr::group_by(!!rlang::sym(variable1)) %>%
            dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
            dplyr::right_join(tmp %>%
              dplyr::select(
                !!rlang::sym(variable1), prob1, prob2,
                DFDR, rr, rd, rr.lcl, rr.ucl, rd.lcl,
                rd.ucl, p, p_adj, AEBODSYS,
                MT_HLT, MT_HLGT,
                bign, TRTA_DetectoR, count
              ) %>%
              dplyr::distinct(), by = variable1
            ) 
        } else {
          tmp <- tmp %>%
            dplyr::select(!!rlang::sym(variable1),count) %>%
            dplyr::group_by(!!rlang::sym(variable1)) %>%
            dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
            dplyr::right_join(tmp %>%
              dplyr::select(
                !!rlang::sym(variable1), prob1, prob2,
                rr, rd, rr.lcl, rr.ucl, rd.lcl, rd.ucl,
                MT_HLT, MT_HLGT,
                p, p_adj, AEBODSYS, bign, TRTA_DetectoR, count
              ) %>%
              dplyr::distinct(), by = variable1
            )
        }
      } else if (input$SI_var3 == "MLG_label") {
        tmp <- tmp %>%
          dplyr::select(-"AEBODSYS") %>%
          dplyr::distinct()
        if (Heatmap_coloring == "p-value" & SI_adjust3 == "DFDR") {
        tmp <- tmp %>%
          dplyr::select(-c(MT_HLT, MT_HLGT)) %>% 
          unique() %>% 
          dplyr::select(!!rlang::sym(variable1),count) %>%
          dplyr::group_by(!!rlang::sym(variable1)) %>%
          dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
          dplyr::right_join(tmp %>%
            dplyr::select(
              !!rlang::sym(variable1),DFDR, prob1, prob2, rr, rd, rr.lcl, rr.ucl, rd.lcl, rd.ucl, p, p_adj, 
              SOC_MLG,
              bign, TRTA_DetectoR, count
            ) %>%
            dplyr::distinct(), by = c(variable1)
          ) 
        } else {
          tmp <- tmp %>%
            dplyr::select(-c(MT_HLT, MT_HLGT)) %>% 
            unique() %>% 
            dplyr::select(!!rlang::sym(variable1),count) %>%
            dplyr::group_by(!!rlang::sym(variable1)) %>%
            dplyr::summarise(Total = sum(count), .groups = 'drop') %>%
            dplyr::right_join(
              tmp %>%
                dplyr::select(
                  !!rlang::sym(variable1),
                  prob1,
                  prob2,
                  rr,
                  rd, 
                  rr.lcl, 
                  rr.ucl,
                  rd.lcl, 
                  rd.ucl,
                  p, 
                  p_adj, 
                  SOC_MLG, 
                  bign, 
                  TRTA_DetectoR,
                  count
                ) %>%
                dplyr::distinct(), by = c(variable1)
            ) 
        }
      }
      tmp
    } else {
      tmp <- NULL
    }
  })
  SI_strat3 <- shiny::reactiveValues(val = "None")
  
  output$SI_var3 <- shiny::renderUI({
    shiny::req(mod_dataUpload_server_vars$meddraVersion())
    if (mod_dataUpload_server_vars$meddraVersion() != 'NoMeddra') {
      shiny::selectInput(
        inputId = ns("SI_var3"),
        label = HTML(
          "<span style = 'color: white;'> Hierarchy </span>"
        ),
        choices = list(
          "SOCs / PTs" = "AEDECOD",
          "SOCs/HLGTs/HLTs/PTs" = "AEDECOD_HLT",
          "SOCS / MLGs" = "MLG_label"
        ),
        selected = "AEDECOD"
      )
    } else {
      shiny::selectInput(
        inputId = ns("SI_var3"),
        label = HTML("<span style = 'color: white;'> Hierarchy </span>"),
        choices = list("SOCs / PTs" = "AEDECOD"),
        selected = "AEDECOD"
      )
    }
  })
  output$SI_disp3 <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("SI_disp3"),
      label = HTML("<span style = 'color: white;'> Effect </span>"),
      choices = list(
        "Relative Risk" = "RR",
        "Risk Difference" = "RD"
      ),
      selected = "RR"
    )
  })
  
  output$Heatmap_coloring <- shiny::renderUI({
    shiny::selectInput(
      inputId = "Heatmap_coloring",
      label = HTML("<span style = 'color: white;'> Heatmap Coloring </span>"),
      choices = list(
        "P-value" = "p-value",
        "Effect" = "effect"
      ),
      selected = "effect"
    )
  })
  
  ####... 58. SI_adjust3 ####
  output$SI_adjust3 <- shiny::renderUI({
    shiny::selectInput(
      inputId = "SI_adjust3",
      label = HTML("<span style = 'color: white;'> p-value adjustment </span>"),
      choices = list(
        "False Discovery Rate" = "FDR",
        "New Double False Discovery Rate" ="DFDR"
      ),
      selected = "FDR"
    )
  })
  
   output$SI_strat3 <- shiny::renderUI({
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
      inputId = "SI_strat3",
      label = HTML("<span style = 'color: white;'> Stratify by </span>"),
      selected = SI_strat3$val,
      choices = choices
    )
  })
  
  shiny::observeEvent(input$SI_strat3, {
    SI_strat3$val <- input$SI_strat3
  })                  
  
  min.event3 <- shiny::reactive({
    shiny::req(alpha3$val, N_1(), N_2())
    tmp <- find.min.event(N1 = N_1(), N2 = N_2(), alpha = as.numeric(alpha3$val))
    tmp
  })
  
  SI_filter_heat <- shiny::reactiveValues(val = "nomethod")
  
  output$SI_filter3 <- shiny::renderUI({
    shiny::req(min.event3())
    minE <- min.event3()
    minEv <- paste0("Use the minimum number of events in Verum (n = ", minE, ")")
    choices <- list("nomethod", "onepercent", "min")
    names(choices) <- c("No Method", "Overall incidence >= 1%", minEv)
    shinyWidgets::prettyRadioButtons(
      inputId = "SI_filter3",
      label = HTML("<span style = 'color: white;'> AE Filtering </span>"),
      choices = choices,
      selected = SI_filter_heat$val,
      fill = TRUE,
      status = "info"
    )
  })
  
  shiny::observeEvent(input$SI_filter3,{
    SI_filter_heat$val <- input$SI_filter3
  })                                     
  alpha3 <- shiny::reactiveValues(val = "0.05")
  
  output$SI_alpha3 <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
      inputId = "SI_alpha3",
      label = HTML("<span style = 'color: white;'> Significance level (alpha) </span>"),
      choices = c("0.01", "0.05", "0.1"),
      selected = alpha3$val,
      fill = TRUE,
      status = "info"
    )
  })
  
  shiny::observeEvent(c(input$SI_alpha3), {
    alpha3$val <- input$SI_alpha3
  })
  
  output$myImage3 <- shiny::renderUI({
    list(shiny::HTML("<img src = 'AppIcon_BAG_DetectoR_210x210mm_RGB.png' alt = 'Graphic cannot be displayed' width = '400' height = '400'>"))
  }) 
  
  tm <- shiny::eventReactive(c(input$apply, input$apply2, input$go_tab3), {
    #requirements and assignments
    shiny::req(heatmapPrep(), alpha3$val)
    
    ges1 <- heatmapPrep()
    tmp <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(!!rlang::sym(strsplit(req(input$SI_var3),"_H")[[1]][1]), TRTA_DetectoR, count) %>% 
      tidyr::pivot_wider(id_cols = !!rlang::sym(strsplit(req(input$SI_var3),"_H")[[1]][1]), values_from = count, names_from = TRTA_DetectoR)
  if(input$SI_var3 == "AEDECOD") {
     tmp2 <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(AEBODSYS, TRTA_DetectoR, count) %>%
      dplyr::group_by(AEBODSYS,TRTA_DetectoR) %>% 
      dplyr::summarise(count = sum(count), .groups = 'drop') %>% 
      tidyr::pivot_wider(id_cols = AEBODSYS, values_from = count, names_from = TRTA_DetectoR) %>% 
      dplyr::rename(labels = AEBODSYS)
  } else if (input$SI_var3 == "MLG_label") {
    tmp2 <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(SOC_MLG, TRTA_DetectoR, count) %>%
      dplyr::group_by(SOC_MLG,TRTA_DetectoR) %>% 
      dplyr::summarise(count = sum(count), .groups = 'drop') %>% 
      tidyr::pivot_wider(id_cols = SOC_MLG, values_from = count, names_from = TRTA_DetectoR) %>% 
      dplyr::rename(labels = SOC_MLG)
  } else if (input$SI_var3 == "AEDECOD_HLT") {
    tmp2 <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(AEBODSYS, TRTA_DetectoR, count) %>%
      dplyr::group_by(AEBODSYS,TRTA_DetectoR) %>% 
      dplyr::summarise(count = sum(count), .groups = 'drop') %>% 
      tidyr::pivot_wider(id_cols = AEBODSYS, values_from = count, names_from = TRTA_DetectoR) %>% 
      dplyr::rename(labels = AEBODSYS)
    tmp3 <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(MT_HLT, TRTA_DetectoR, count) %>%
      dplyr::group_by(MT_HLT,TRTA_DetectoR) %>% 
      dplyr::summarise(count = sum(count), .groups = 'drop') %>% 
      tidyr::pivot_wider(id_cols = MT_HLT, values_from = count, names_from = TRTA_DetectoR) %>% 
      dplyr::rename(labels = MT_HLT)
    tmp4 <- ges1 %>% 
      dplyr::filter(Total != 0) %>% 
      dplyr::select(MT_HLGT, TRTA_DetectoR, count) %>%
      dplyr::group_by(MT_HLGT,TRTA_DetectoR) %>% 
      dplyr::summarise(count = sum(count), .groups = 'drop') %>% 
      tidyr::pivot_wider(id_cols = MT_HLGT, values_from = count, names_from = TRTA_DetectoR) %>% 
      dplyr::rename(labels = MT_HLGT)
    colnames(tmp3)[2] <- strsplit(colnames(tmp3)," ")[[2]][1]
    colnames(tmp3)[3] <- strsplit(colnames(tmp3)," ")[[3]][1]
    colnames(tmp4)[2] <- strsplit(colnames(tmp4)," ")[[2]][1]
    colnames(tmp4)[3] <- strsplit(colnames(tmp4)," ")[[3]][1]
    verum_comp3 <- tmp3 %>%
      dplyr::mutate(ratio = paste0("(", Verum, "/", Comparator, ")")) 
    verum_comp4 <- tmp4 %>%
      dplyr::mutate(ratio = paste0("(", Verum, "/", Comparator, ")")) 
  }
    
    if (length(colnames(tmp)) == 3) {
    colnames(tmp)[2] <- strsplit(colnames(tmp)," ")[[2]][1]
    colnames(tmp)[3] <- strsplit(colnames(tmp)," ")[[3]][1]
    
    colnames(tmp2)[2] <- strsplit(colnames(tmp2)," ")[[2]][1]
    colnames(tmp2)[3] <- strsplit(colnames(tmp2)," ")[[3]][1]
    
    verum_comp <- tmp %>%
      dplyr::mutate(ratio = paste0("(", Verum, "/", Comparator, ")")) %>% 
      dplyr::rename(labels = !!rlang::sym(strsplit(req(input$SI_var3),"_H")[[1]][1]))

    verum_comp2 <- tmp2 %>%
      dplyr::mutate(ratio = paste0("(", Verum, "/", Comparator, ")")) 

    if (input$SI_var3 != "AEDECOD_HLT") {
      verum_comp_tot <- dplyr::bind_rows(verum_comp, verum_comp2)
    } else {
      verum_comp_tot <- dplyr::bind_rows(verum_comp, verum_comp2, verum_comp3, verum_comp4)
    }
    
    #remove duplicated rows and empty groups
    ges1 <- ges1 %>%
      dplyr::select(-bign,-TRTA_DetectoR, -count) %>%
      unique() %>% 
      dplyr::filter(Total != 0)

    alph <- as.numeric(alpha3$val)
    filt <- SI_filter_heat$val
    colpal <- c(
      "#1E90FF", "#278BF5", "#3087EC", "#3982E3", "#427EDA", "#4B7AD1", "#5475C7",
      "#5D71BE", "#666DB5", "#6F68AC", "#7864A3", "#81609A", "#8A5B90", "#935787",
      "#9C537E", "#A54E75", "#AE4A6C", "#B74663", "#C04159", "#C93D50", "#D23947",
      "#DB343E", "#E43035"
    )
    
    if (input$heatmap_Selection == "RR") {
      if (dim(ges1)[1] > 1) { 
        
 
      vec <- round(exp(seq(log(1), log(base::max(ges1$rr)), length = 13)), 2)
      ges1 <- ges1 %>%
       dplyr::mutate(
         color = dplyr::case_when(
           rr <= 1 ~ colpal[1],
           rr > vec[1] & rr <=vec[2] ~colpal[12],
           rr > vec[2] & rr <=vec[3] ~colpal[13],
           rr > vec[3] & rr <=vec[4] ~colpal[14],
           rr > vec[4] & rr <=vec[5] ~colpal[15],
           rr > vec[5] & rr <=vec[6] ~colpal[16],
           rr > vec[6] & rr <=vec[7] ~colpal[17],
           rr > vec[7] & rr <=vec[8] ~colpal[18],
           rr > vec[8] & rr <=vec[9] ~colpal[19],
           rr > vec[9] & rr <=vec[10] ~colpal[20],
           rr > vec[10] & rr <=vec[11] ~colpal[21],
           rr > vec[11] & rr <=vec[12] ~colpal[22],
           rr > vec[12]
           ~colpal[23]
          )
        )
      }
      target_name <- "rr"
      target_variable <- ges1$rr
      
      if (input$SI_var3 == "AEDECOD") {
        ges1 <- ges1 %>%
          dplyr::mutate(
            parents = AEBODSYS, 
            labels = AEDECOD
          )
        ges1_add <- ges1 %>%
          dplyr::group_by(parents) %>% 
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
          dplyr::rename(labels = parents) %>% 
          dplyr::mutate(
            parents = "",
            rr = "") %>% 
          as.data.frame()
        
      } else if (input$SI_var3 == "MLG_label") {
        ges1 <- ges1 %>%
          dplyr::mutate(
            parents = SOC_MLG, 
            labels = MLG_label
          )
        
        ges1_add <- ges1 %>%
          dplyr::group_by(parents) %>% 
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
          dplyr::rename(labels = parents) %>% 
          dplyr::mutate(parents = "",
            rr = "") %>% 
          as.data.frame()
        
      } else if (input$SI_var3 == "AEDECOD_HLT") {
        
        ges1_ <- ges1 %>%
          dplyr::mutate(
            parents = MT_HLT,
            labels =  AEDECOD
          )
        ges1_add <- ges1_ %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 1,
            rr = "") %>%
          dplyr::rename(labels = parents) %>%
          as.data.frame()
        
        ges1_add_ <- ges1_ %>%
         dplyr::select(MT_HLT,MT_HLGT) %>%
          dplyr::rename(labels = MT_HLT, parents = MT_HLGT)
        
        ges1_add <- ges1_add %>%
          dplyr::right_join(ges1_add_, by = "labels")
        

        ges2 <- ges1 %>%
          dplyr::mutate(
            parents = MT_HLGT,
            labels =  MT_HLT
          )
        ges2_add <- ges2 %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 1,
            rr = "") %>%
          dplyr::rename(labels = parents) %>%
          as.data.frame()

        ges2_add_ <- ges2 %>%
         dplyr::select(MT_HLGT,AEBODSYS) %>%
          dplyr::rename(labels = MT_HLGT, parents = AEBODSYS)

        ges2_add <- ges2_add %>%
          dplyr::right_join(ges2_add_, by = "labels")

        ges3 <- ges1 %>%
          dplyr::mutate(
            parents = AEBODSYS,
            labels =  MT_HLGT
          )
        
        ges3_add <- ges3 %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0,
            rr = "") %>%
          dplyr::rename(labels = parents) %>%
          dplyr::mutate(parents = "") %>%
          as.data.frame()
      }
      
      if (input$SI_var3 == "AEDECOD_HLT") {
        ges1_base <- ges1_ %>% 
          dplyr::rename(vSize = Total) %>%
          dplyr::mutate(vSize2 = vSize) %>% 
          dplyr::select(parents, labels, color, vSize, vSize2,rr)
        
        ges1_tot <- rbind(ges1_base, ges1_add, ges2_add, ges3_add) 
      } else {
        ges1_base <- ges1 %>% 
          dplyr::rename(vSize = Total) %>%
          dplyr::mutate(vSize2 = vSize) %>% 
          dplyr::select(parents,labels, color, vSize, vSize2,rr)
        if (".groups" %in% colnames(ges1_add)){
          ges1_add <- ges1_add %>% 
            dplyr::select(-'.groups')
        } 
        if (".groups" %in% colnames(ges1_base)){
          ges1_add <- ges1_base %>% 
            dplyr::select(-'.groups')
        } 
        ges1_tot <- rbind(ges1_add, ges1_base)
      }
    } else if (input$heatmap_Selection == "RD") {
      
      vec <- seq(0, base::max(ges1$rd), length = 13)
      #vec <- round(seq(0, base::max(ges1$rd), length = 13), 2)
      ges1 <- ges1 %>%
        dplyr::mutate(
          color = dplyr::case_when(
            rd <= 0 ~ colpal[1],
            rd > vec[1] & rd <=vec[2] ~colpal[12],
            rd > vec[2] & rd <=vec[3] ~colpal[13],
            rd > vec[3] & rd <=vec[4] ~colpal[14],
            rd > vec[4] & rd <=vec[5] ~colpal[15],
            rd > vec[5] & rd <=vec[6] ~colpal[16],
            rd > vec[6] & rd <=vec[7] ~colpal[17],
            rd > vec[7] & rd <=vec[8] ~colpal[18],
            rd > vec[8] & rd <=vec[9] ~colpal[19],
            rd > vec[9] & rd <=vec[10] ~colpal[20],
            rd > vec[10] & rd <=vec[11] ~colpal[21],
            rd > vec[11] & rd <=vec[12] ~colpal[22],
            rd > vec[12] ~colpal[23]
          )
        )
      target_name <- "rd"
      target_variable <- ges1$rd
      
      if (input$SI_var3 == "AEDECOD") {
        ges1_ <- ges1 %>%
          dplyr::mutate(
            parents = AEBODSYS, 
            labels = AEDECOD
          )
        ges1_add <- ges1_ %>%
          dplyr::group_by(parents) %>% 
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
          dplyr::rename(labels = parents) %>% 
          dplyr::mutate(parents = "",
            rd = "") %>% 
          as.data.frame()
      } else if (input$SI_var3 == "MLG_label") {
        ges1_ <- ges1 %>%
          dplyr::mutate(
            parents = SOC_MLG, 
            labels = MLG_label
          )
        ges1_add <- ges1_ %>%
          dplyr::group_by(parents) %>% 
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
          dplyr::rename(labels = parents) %>% 
          dplyr::mutate(parents = "",
            rd = "") %>% 
          as.data.frame()
      } else if (input$SI_var3 == "AEDECOD_HLT") {
        ges1_ <- ges1 %>%
          dplyr::mutate(
            parents = MT_HLT, 
            labels = AEDECOD
          )
      }
      
      if (input$SI_var3 == "AEDECOD_HLT") {
        ges1_ <- ges1 %>%
          dplyr::mutate(
            parents = MT_HLT,
            labels =  AEDECOD
          )
        ges1_add <- ges1_ %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 1,
            rd = "") %>%
          dplyr::rename(labels = parents) %>%
          as.data.frame()
        
        ges1_add_ <- ges1_ %>%
         dplyr::select(MT_HLT,MT_HLGT) %>%
         dplyr::rename(labels = MT_HLT, parents = MT_HLGT)
        
        ges1_add <- ges1_add %>%
          dplyr::right_join(ges1_add_, by = "labels")
        
        ges2 <- ges1 %>%
          dplyr::mutate(
            parents = MT_HLGT,
            labels =  MT_HLT
          )
        
        ges2_add <- ges2 %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 1,
            rd = "") %>%
          dplyr::rename(labels = parents) %>%
          as.data.frame()
        
        ges2_add_ <- ges2 %>%
         dplyr::select(MT_HLGT,AEBODSYS) %>%
         dplyr::rename(labels = MT_HLGT, parents = AEBODSYS)
        
        ges2_add <- ges2_add %>%
          dplyr::right_join(ges2_add_, by = "labels")
        
        ges3 <- ges1 %>%
          dplyr::mutate(
            parents = AEBODSYS,
            labels =  MT_HLGT
          )
        
        ges3_add <- ges3 %>%
          dplyr::group_by(parents) %>%
          dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
          dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
          dplyr::rename(labels = parents) %>%
          dplyr::mutate(parents = "",
            rd = "") %>%
          as.data.frame()
      }
      
      if (input$SI_var3 == "AEDECOD_HLT") {
        ges1_base <- ges1_ %>% 
          dplyr::rename(vSize = Total) %>%
          dplyr::mutate(vSize2 = vSize) %>% 
          dplyr::select(parents,labels, color, vSize, vSize2, rd)
        ges1_tot <- rbind(ges1_base, ges1_add, ges2_add, ges3_add) 
      } else {
        ges1_base <- ges1_ %>%
          dplyr::rename(vSize = Total) %>%
          dplyr::mutate(vSize2 = vSize) %>%
          dplyr::select(parents,labels, color, vSize, vSize2, rd)
        ges1_tot <- rbind(ges1_add, ges1_base)
      }
    } else {
      if (input$heatmap_Selection == "FDR") {
        
        SI_adjust3 <- "p_adj"
        target_variable <- ges1$p_adj
      } else if (input$heatmap_Selection == "DFDR") {
        target_variable <- ges1$DFDR
        SI_adjust3 <- "DFDR"
      }

      if (alpha3$val == "0.05") {
        ges1 <- ges1 %>%
         dplyr::mutate(
           color = dplyr::case_when(
             !!rlang::sym(SI_adjust3) > 0.05 ~ colpal[1],
             !!rlang::sym(SI_adjust3)> 0.01 &!!rlang::sym(SI_adjust3)<= 0.05 ~colpal[11],
             !!rlang::sym(SI_adjust3)> 0.001 &!!rlang::sym(SI_adjust3)<= 0.01 ~colpal[14],
             !!rlang::sym(SI_adjust3)> 0.0001 &!!rlang::sym(SI_adjust3)<=0.001 ~colpal[17],
             !!rlang::sym(SI_adjust3)> 0.00001 &!!rlang::sym(SI_adjust3)<=0.0001 ~colpal[21],
             !!rlang::sym(SI_adjust3)<=0.00001 ~ colpal[23],
             is.na(!!rlang::sym(SI_adjust3))~ "#424242"
          )
        )
      } else if (alpha3$val == "0.01") {
        ges1 <- ges1 %>%
          dplyr::mutate(
            color = dplyr::case_when(
              !!rlang::sym(SI_adjust3) > 0.01 ~ colpal[1],
              !!rlang::sym(SI_adjust3)> 0.001 &!!rlang::sym(SI_adjust3)<= 0.01 ~colpal[11],
              !!rlang::sym(SI_adjust3)> 0.0001 &!!rlang::sym(SI_adjust3)<=0.001 ~colpal[15],
              !!rlang::sym(SI_adjust3)> 0.00001 &!!rlang::sym(SI_adjust3)<=0.0001 ~colpal[19],
              !!rlang::sym(SI_adjust3)<=0.00001 ~ colpal[23],
              is.na(!!rlang::sym(SI_adjust3))~ "#424242"
            )
          )
        
      } else if (alpha3$val == "0.1") {
          ges1 <- ges1 %>%
           dplyr::mutate(
             color = dplyr::case_when(
               !!rlang::sym(SI_adjust3) > 0.1 ~ colpal[1],
               !!rlang::sym(SI_adjust3)> 0.01 &!!rlang::sym(SI_adjust3)<= 0.1 ~colpal[11],
               !!rlang::sym(SI_adjust3)> 0.001 &!!rlang::sym(SI_adjust3)<= 0.01 ~colpal[14],
               !!rlang::sym(SI_adjust3)> 0.0001 &!!rlang::sym(SI_adjust3)<=0.001 ~colpal[17],
               !!rlang::sym(SI_adjust3)> 0.00001 &!!rlang::sym(SI_adjust3)<=0.0001 ~colpal[21],
               !!rlang::sym(SI_adjust3)<=0.00001 ~ colpal[23],
               is.na(!!rlang::sym(SI_adjust3))~ "#424242"
              )
            )
      }
        if (input$SI_var3 == "AEDECOD") {
          ges1 <- ges1 %>%
            dplyr::mutate(
              parents = AEBODSYS, 
              labels = AEDECOD
            )
          ges1_add <- ges1 %>%
            dplyr::group_by(parents) %>% 
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
            dplyr::rename(labels = parents) %>% 
            dplyr::mutate(
              parents = "",
              p_adj = "", DFDR = ""
            ) %>% 
            as.data.frame()
        } else if (input$SI_var3 == "MLG_label") {
          ges1 <- ges1 %>%
            dplyr::mutate(
              parents = SOC_MLG, 
              labels = MLG_label
            )
          ges1_add <- ges1 %>%
            dplyr::group_by(parents) %>% 
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
            dplyr::rename(labels = parents) %>% 
            dplyr::mutate(
              parents = "",
              p_adj = "", DFDR = ""
            ) %>% 
            as.data.frame()
        } else if (input$SI_var3 == "AEDECOD_HLT") {
          
          ges1_ <- ges1 %>%
            dplyr::mutate(
              parents = MT_HLT, 
              labels = AEDECOD
            )
          
          ges1_add <- ges1_ %>%
            dplyr::group_by(parents) %>% 
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
            dplyr::rename(labels = parents) %>% 
            dplyr::mutate(
              parents = "",
              p_adj = "", DFDR = ""
            ) %>% 
            as.data.frame()
        }
        
    
        
        if(input$SI_var3 == "AEDECOD_HLT") {
          ges1_add <- ges1_ %>%
            dplyr::group_by(parents) %>%
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(
              color = "#4d4d4d",
              vSize2 = 1,
              p_adj = "", DFDR = ""
            ) %>%
            dplyr::rename(labels = parents) %>%
            as.data.frame()
          
          ges1_add_ <- ges1_ %>%
            dplyr::select(MT_HLT,MT_HLGT) %>%
            dplyr::rename(
              labels = MT_HLT,
              parents = MT_HLGT
            )
          
          ges1_add <- ges1_add %>%
            dplyr::right_join(ges1_add_, by = "labels")
          
          
          ges2 <- ges1 %>%
            dplyr::mutate(
              parents = MT_HLGT,
              labels =  MT_HLT
            )
          ges2_add <- ges2 %>%
            dplyr::group_by(parents) %>%
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(color = "#4d4d4d", vSize2 = 1,
              p_adj = "", DFDR ="") %>%
            dplyr::rename(labels = parents) %>%
            as.data.frame()
          
          ges2_add_ <- ges2 %>%
            dplyr::select(MT_HLGT,AEBODSYS) %>%
            dplyr::rename(labels = MT_HLGT, parents = AEBODSYS)
          
          ges2_add <- ges2_add %>%
            dplyr::right_join(ges2_add_, by = "labels")
          
          ges3 <- ges1 %>% 
            dplyr::mutate(
              parents = AEBODSYS,
              labels =  MT_HLGT
            )
          ges3_add <- ges3 %>%
            dplyr::group_by(parents) %>%
            dplyr::summarise(vSize = sum(Total), .groups = 'drop') %>%
            dplyr::mutate(color = "#4d4d4d", vSize2 = 0) %>%
            dplyr::rename(labels = parents) %>%
            dplyr::mutate(parents = "",
              p_adj = "", DFDR = "") %>%
            as.data.frame()
        }
        
       
        if (input$SI_var3 == "AEDECOD_HLT") {
          
          ges1_base <- ges1_ %>% 
            dplyr::rename(vSize = Total) %>% 
            dplyr::mutate(vSize2 = vSize) %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3))
          ges1_tot <- rbind(ges1_base %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3))
            ,ges1_add %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3)),
            ges2_add %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3)), 
            ges3_add %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3))) 
        } else {
          ges1_base <- ges1 %>%
            dplyr::rename(vSize = Total) %>%
            dplyr::mutate(vSize2 = vSize) %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3))
          ges1_tot <- rbind(ges1_add %>% 
            dplyr::select(parents,labels, color, vSize, vSize2, !!rlang::sym(SI_adjust3)), ges1_base)
        }
      target_name <- "p:value"
      
    }
    
    ges1_tot$labels <- as.character(ges1_tot$labels)
    ges1_tot$parents <- as.character(ges1_tot$parents)
    
    ges1_tot <- ges1_tot %>% 
      dplyr::right_join(verum_comp_tot, by = "labels")
    ges1_tot
    } else {
      ges1_tot <- tibble::tibble(
        labels = character(),
        Comparator = numeric(),
        Verum = numeric(),
        ratio = character()
      )
      ges1_tot
    }
  })

  output$tree1 <- shiny::renderUI({
    tm()
    plotly::plotlyOutput(ns("tree")) %>%
      shinycssloaders::withSpinner(type = 4)
  })
  

  output$tree <- plotly::renderPlotly({
     tm()
    input$go_tab3
    version <- packageVersion("plotly")
    validate(
      need(as.numeric(unlist(strsplit(as.character(version),"[.]"))[2]) >= 4 & as.numeric(unlist(strsplit(as.character(version),"[.]"))[2])>=9,
        paste0(
          "plotly package version 4.9.2 is required for plotly type 'treemap'.\n",
          "plotly package version of R Session is: ",
          as.character(version), ".\n",
          "Please update the package to use Heatmap."
        )
      )
    )
    
    ges1_tot <- tm()
    t_1 <- unique(ges1_tot)
    
    if (length(t_1$labels) != 0) {
    
    shiny::validate(
      shiny::need(
        dim(t_1 %>%
          dplyr::filter(labels %in% sort(t_1$labels)[which((sort(t_1$labels)[1:(length(t_1$labels)-1)] == sort(t_1$labels)[2:length(t_1$labels)]))])
        )[1] == 0,
        paste0(
          "WARNING: Not all 'labels' have unique parent values! Following values have multiple parent values: label - '",
          (t_1 %>%
              dplyr::filter(
                labels %in% sort(t_1$labels)[which((sort(t_1$labels)[1:(length(t_1$labels)-1)] == sort(t_1$labels)[2:length(t_1$labels)]))]
              )
          )$labels,
          "' has parent value - '",
          (t_1 %>%
            dplyr::filter(
              labels %in% sort(t_1$labels)[which((sort(t_1$labels)[1:(length(t_1$labels)-1)] == sort(t_1$labels)[2:length(t_1$labels)]))]
            )
          )$parents,
          "'"
        )
      )
    )
    ges1_tot <- unique(ges1_tot)
    
    if (shiny::isolate(input$heatmap_Selection) == "RR") {
      ges2_tot <- ges1_tot %>%
        dplyr::mutate(
        rr2 = dplyr::case_when(
          rr != "" ~ as.character(round(as.numeric(rr), 2)),
          rr == "" ~ ""
        )
      )
      plotly_text <- paste(
        '</br> Total: ', ges2_tot$vSize,
        '</br> (Verum/Comparator): ', ges2_tot$ratio,
        '</br> RR: ', ges2_tot$rr2
      )
    } else if (shiny::isolate(input$heatmap_Selection) == "RD") {
      ges2_tot <- ges1_tot %>%
        dplyr::mutate(
        rd2 = dplyr::case_when(
          rd != "" ~ as.character(round(as.numeric(rd), 4)),
          rd == "" ~ ""
        )
      )
      plotly_text <- paste(
        '</br> Total: ', ges2_tot$vSize,
        '</br> (Verum/Comparator): ', ges2_tot$ratio,
        '</br> RD: ', ges2_tot$rd2
      )
    } else if (shiny::isolate(input$heatmap_Selection) == "FDR") {
       ges2_tot <- ges1_tot %>%
         dplyr::mutate(
          p_adj2 = dplyr::case_when(
          p_adj != "" ~ as.character(round(as.numeric(p_adj), 4)),
          p_adj == "" ~ ""
        )
      )
       
      plotly_text <- paste(
        '</br> Total: ', ges2_tot$vSize,
        '</br> (Verum/Comparator): ', ges2_tot$ratio,
        '</br> adjusted p-value: ', ges2_tot$p_adj2
      )
    } else if (shiny::isolate(input$heatmap_Selection) == "DFDR") {
       ges2_tot <- ges1_tot %>%
        dplyr::mutate(
          DFDR2 = dplyr::case_when(
          DFDR != "" ~ as.character(round(as.numeric(DFDR), 4)),
          DFDR == "" ~ ""
        )
      )
      
      plotly_text <- paste(
        '</br> Total: ', ges2_tot$vSize,
        '</br> (Verum/Comparator): ', ges2_tot$ratio,
        '</br> adjusted p-value: ', ges2_tot$DFDR2
      )
    }
     
    
    # This version of the R package upgrades the version of the
    # underlying plotly.js library from v1.49.4 to v1.52.2.
    # This includes many bug fixes, improvements, as well as
    # 2 new trace types: treemap and image. 
    # The plotly.js release page has the full list of changes.
    
      fig <- plotly::plot_ly(
        type = 'treemap',
        labels = ges1_tot$labels,
        parents = ges1_tot$parents,
        values = ges1_tot$vSize2,
        marker = list(colors = ges1_tot$color),
        hovertemplate = '<b>%{label} </b> <extra></extra>',
        pathbar = list(visible = FALSE),
        text = ~ plotly_text,
      ) %>%
        plotly::layout(plot_bgcolor = '#424242') %>%
        plotly::layout(paper_bgcolor = '#424242')
    fig 
    }
  })
  
  output$legend <- renderPlot({
    # requirements and assignments
    shiny::req(heatmapPrep())
    
    heatmapPrep <- heatmapPrep()
    colpal <- c("#1E90FF", "#278BF5", "#3087EC", "#3982E3", "#427EDA", "#4B7AD1", "#5475C7",
                "#5D71BE", "#666DB5", "#6F68AC", "#7864A3", "#81609A", "#8A5B90", "#935787",
                "#9C537E", "#A54E75", "#AE4A6C", "#B74663", "#C04159", "#C93D50", "#D23947",
                "#DB343E", "#E43035")
    
     if(isolate(input$heatmap_Selection) == "RR") {
       SI_disp3 <- "RR"
       Heatmap_coloring <- "effect"
       SI_adjust3 <- "FDR"
     } else if (isolate(input$heatmap_Selection) == "RD") {
       SI_disp3 <- "RD"
       Heatmap_coloring <- "effect"
       SI_adjust3 <- "FDR"
     } else if (isolate(input$heatmap_Selection) == "FDR") {
       SI_disp3 <- "RD"
       Heatmap_coloring <- "p-value"
       SI_adjust3 <- "FDR"
     } else if (isolate(input$heatmap_Selection) == "DFDR") {
       SI_disp3 <- "RD"
       Heatmap_coloring <- "p-value"
       SI_adjust3 <- "DFDR"
     }
    
    if (shiny::isolate(Heatmap_coloring) != "effect") {
      #p-value Range from 0 to 1
      legendRange1 <- 0
      legendRange2 <- 1
    } else {
      if(dim(heatmapPrep)[1] > 0) {
        #get the range from the effect value (either rr or rd)
        if(SI_disp3 == "RR"){
          legendRange1 <- heatmapPrep$rr %>%
            base::min(na.rm =TRUE)
          legendRange2 <- heatmapPrep$rr %>%
            base::max(na.rm =TRUE)
        }
        else if(SI_disp3 =="RD"){
          legendRange1 <- heatmapPrep$rd %>%
            base::min(na.rm =TRUE)
          legendRange2 <- heatmapPrep$rd %>%
            base::max(na.rm =TRUE)
        }
      }
    }
    # assign plot middle and width
    leg.x <- 0.5
    leg.width <- 1
    
    if(isolate(Heatmap_coloring) == "effect") {
      
      leg.y <- c(0,seq(0.5, 1, length.out = 13))
      
      #Create an empty plot
      par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
      
      plot(NULL, NULL, ylim = c(0,1), xlim = c(leg.y[1],leg.y[length(leg.y)]), axes = FALSE, ylab = "", xlab = "")
      rect(ybottom = -2,ytop = 2,xleft = -2,xright=2,col= col_darkgrey)
      #Draw the legend bar
      rect(
        ybottom = leg.x - 2,
        ytop = leg.x + 2,
        xleft = leg.y[-1],
        xright = leg.y[-length(leg.y)],
        xpd = NA,
        col = c(
          colpal[1],
          colpal[12],
          colpal[13],
          colpal[14],
          colpal[15],
          colpal[16],
          colpal[17],
          colpal[18],
          colpal[19],
          colpal[20],
          colpal[21],
          colpal[22],
          colpal[23]
        ),
        border=TRUE
      )
      #Write labels in the small colored boxes
      if (SI_disp3 == "RR") {
        text(
          c(0.25, seq(0.52, 0.9783333, length = 12)),
          leg.x,
          c("<=1",round(exp(seq(log(1), log(legendRange2), length = 13)), 2)[-1]),
          col = c('white')
        )
      }else if(SI_disp3 =="RD"){
        vec <- seq(0, legendRange2, length = 13)
        if (all(round(vec,2) == 0)) {
          vec <- signif(vec, digits = 2)
        }
        text(
          c(0.25, seq(0.52, 0.9783333, length = 12)),
          leg.x,
          c("<=0", vec[-1]),
          col = c('white')
        )
      }
    } else {
      
      leg.y <- c(0, seq(0.5, 1, length.out = 13))
      
      #Create an empty plot
      par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
      
      plot(
        NULL, 
        NULL, 
        ylim = c(0, 1),
        xlim = c(leg.y[1], leg.y[length(leg.y)]),
        axes = FALSE,
        ylab = "",
        xlab = ""
      )
      rect(
        ybottom = -2,
        ytop = 2, 
        xleft = -2, 
        xright = 2,
        col = col_darkgrey
      )
      #Draw the legend bar
      if (shiny::isolate(alpha3$val) == "0.01") {
        leg.y <- seq(0, 1, length = 6)
        rect(
          ybottom = leg.x - 2,
          ytop = leg.x + 2,
          xleft = leg.y[-1],
          xright = leg.y[-length(leg.y)],
          xpd = NA,
          col = c(colpal[23], colpal[19], colpal[15], colpal[11], colpal[1]),
          border = TRUE
        )
      } else if (isolate(alpha3$val) %in% c("0.05", "0.1")) {
        leg.y <- seq(0, 1, length = 7)
        rect(
          ybottom = leg.x - 2,
          ytop = leg.x + 2,
          xleft = leg.y[-1],
          xright = leg.y[-length(leg.y)],
          xpd = NA,
          col = c(colpal[23], colpal[20], colpal[17], colpal[14], colpal[11], colpal[1]),
          border = TRUE
        )
      }
      #Write labels in the small colored boxes
      
      if(isolate(alpha3$val) == "0.05") {
        text(
          seq(0.07, 0.93, length = 6),
          0.5, c("<0.00001", "(0.00001,0.0001]", "(0.0001,0.001]", "(0.001,0.01]", "(0.01,0.05]", "(0.05,1]"),
          col = c('white')
        )
      } else if (shiny::isolate(alpha3$val) == "0.01") {
        text(
          seq(0.07, 0.93, length = 5),
          0.5,
          c("<0.00001", "(0.00001,0.0001]", "(0.0001,0.001]", "(0.001,0.01]", "(0.01,1]"),
          col = c('white')
        )
      } else if(isolate(alpha3$val) == "0.1") {
        text(
          seq(0.07, 0.93, length = 6),
          0.5,
          c("<0.00001", "(0.00001,0.0001]", "(0.0001,0.001]", "(0.001,0.01]", "(0.01,0.1]", "(0.1,1]"),
          col = c('white')
        )
      }
    }
  }, height = 20)
  
  output$legendPlot <- shiny::renderUI({
    plotOutput(
      outputId = ns('legend'),
      height = 20
    )
  })
  
  ####... 66. arrowsPlot ####
  output$arrows <- shiny::renderPlot({

    input$go_tab3
    
    if (shiny::isolate(input$heatmap_Selection) == "RR") {
      SI_disp3 <- "RR"
      Heatmap_coloring <- "effect"
      SI_adjust3 <- "FDR"
    } else if (shiny::isolate(input$heatmap_Selection) == "RD") {
      SI_disp3 <- "RD"
      Heatmap_coloring <- "effect"
      SI_adjust3 <- "FDR"
    } else if (shiny::isolate(input$heatmap_Selection) == "FDR") {
      SI_disp3 <- "RD"
      Heatmap_coloring <- "p-value"
      SI_adjust3 <- "FDR"
    } else if (shiny::isolate(input$heatmap_Selection) == "DFDR") {
      SI_disp3 <- "RD"
      Heatmap_coloring <- "p-value"
      SI_adjust3 <- "DFDR"
    }
    if (shiny::isolate(Heatmap_coloring) == "effect") {
      par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(0,0,0,0))
      plot(
        NULL,
        xlim = c(0, 10),
        ylim = c(4, 8),
        xlab = "",
        ylab = "",
        axes = FALSE
      )
      rect(
        ybottom = -2,
        ytop = 12, 
        xleft = -2,
        xright = 12,
        col = col_darkgrey
      )
      
      shiny::req(heatmapPrep())
      heatmapPrep <- heatmapPrep()
      if (SI_disp3 =="RR") {
        legendRange1 <- heatmapPrep$rr %>%
          base::min(na.rm = TRUE)
        legendRange2 <- heatmapPrep$rr %>%
          base::max(na.rm = TRUE)
      }
      else if (SI_disp3 =="RD") {
        legendRange1 <- heatmapPrep$rd %>%
          base::min(na.rm = TRUE)
        legendRange2 <- heatmapPrep$rd %>%
          base::max(na.rm = TRUE)
      }
      if (SI_disp3 == "RR") {
        test_seq <- exp(seq(log(legendRange1), log(legendRange2), length = 24))
        mid_point <- 10/24 * which.min(abs(test_seq-1)) - 0.2
      } else if (SI_disp3 == "RD") {
        test_seq <- seq(legendRange1, legendRange2, length = 24)
        mid_point <- 10/24 * which.min(abs(test_seq)) - 0.2
      }
      
      arrows(5 + 0.05, 5, 10, 5,lwd = 4, col = "white", bg = "black")
      arrows(5- 0.05, 5, 0, 5, lwd = 4, col = "white", bg = "black")
      text(0.5, 7, "favours Verum", col = "white", cex = 1.5)
      text(9.5, 7, "favours Comparator", col = "white", cex = 1.5)
      
    } else {
      par(mfrow = c(1,1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
      plot(NULL, xlim = c(0,10), ylim = c(4,8), xlab = "", ylab =" ", axes = FALSE)
      rect(ybottom = -2,
           ytop = 12,
           xleft = -2,
           xright = 12,
           col = col_darkgrey
      )
    }
  }, height = 50)
  
  output$arrowsPlot <- shiny::renderUI({
    shiny::plotOutput(
      outputId = ns('arrows'),
      height = 50
    )
  })

}

## To be copied in the UI
# mod_heatmap_ui("heatmap_ui_1")

## To be copied in the server
# callModule(mod_heatmap_server, "heatmap_ui_1")
