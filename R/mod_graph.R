#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_graph_ui <- function(id) {
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
        inputId = ns("showPanel2"),
        label_off = HTML("<span style='color: white; font-size: 16px;'> Select main options </span>"),
        label_on = HTML("<span style = 'color: white; font-size: 16px;'> Select main options </span>"),
        value = TRUE,
        outline = TRUE,
        status_on = "default",
        status_off = "default",
        plain = TRUE,
        icon_off = icon("cogs"),
        icon_on = icon("cogs")
      ),
      conditionalPanel(
        condition = paste0('input[\'', ns('showPanel2'), "\']"),
        shiny::fluidRow(
          shiny::column(
            1,
            shiny::actionButton(
              inputId = ns("go_tab2"),
              label = "Apply!",
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
            shiny::uiOutput(ns("SI_disp2"))
          ),
          shiny::column(
            2,
            shiny::uiOutput(ns("SI_order_by2"))
          ),
          shiny::column(
            2,
            shiny::uiOutput(ns("SI_adjust"))
          ),
          shiny::column(
            1,
            shinyWidgets::materialSwitch(
              inputId = ns("switch2"),
              label = HTML("<span style = 'color: white;'> Advanced Settings </span>"),
              status = "success",
              value = FALSE
            )
          )
        ),
        shiny::conditionalPanel(
          condition = "output.advanced2 == true",
          shiny::fluidRow(
            shiny::column(
              1,
              shiny::uiOutput(ns("SI_strat2"))
            ),
            shiny::column(
              2,
              shiny::uiOutput(ns("DDP_nrows"))
            ),
            shiny::column(
              2,
              shiny::uiOutput(ns("SI_fisher_alternative"))
            ),
            shiny::column(
              4,
              shiny::uiOutput(ns("SI_filter2"))
            ),
            shiny::column(
              2,
              shinyWidgets::prettyToggle(
                inputId = ns("showLabel"),
                label_off = HTML("<span style='color: white;'> Show full Labels </span>"),
                label_on = HTML("<span style = 'color: white;'> Collapse Labels </span>"),
                value = TRUE,
                outline = TRUE,
                status_on = "default",
                status_off = "default",
                plain = TRUE,
                icon_off = icon("align-right"),
                icon_on = icon("text-width")
              )
            ),
            shiny::column(
              1,
              shiny::uiOutput(ns("SI_alpha2"))
            )
          )
        )
      )
    ),
    tags$style(".fa-align-right {color:#ffffff}"),
    tags$style(".fa-text-width {color:#ffffff}"),
    shiny::conditionalPanel(
      condition = "output.flag == false && output.flag2 == false",
      h2("Welcome to DetectoR"),
      shiny::uiOutput(ns("myImage2")),
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
      shiny::uiOutput(ns("error_message_adae"))
    ),
    shiny::conditionalPanel(condition = "output.flag == true && output.flag2 == true",
      shiny::column(6,
        shiny::wellPanel(style = paste0("background-color: ", col_midgrey, "; border-color: ", col_lightgrey, "; border: 5px; border-radius: 5px;"),
          shinyWidgets::prettyToggle(
            inputId = ns('showhelptextPanel2_1'),
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
          shiny::conditionalPanel(condition = paste0('input[\'', ns('showhelptextPanel2_1'), "\']"),
            shiny::uiOutput(
              ns('tot_info2_1')
            )
          )
        )
      ),
       shiny::column(6,
        shiny::wellPanel(style = paste0("background-color: ", col_midgrey, "; border-color: ", col_lightgrey, "; border: 5px; border-radius: 5px;"),
          shinyWidgets::prettyToggle(
            inputId = ns('showhelptextPanel2_3'),
            label_off = HTML("<span style='color: white; font-size: 16px;'> Study Sites Information </span>"),
            label_on = HTML("<span style = 'color: white; font-size: 16px;'> Study Sites Information </span>"),
            value = FALSE,
            outline = TRUE,
            status_on = "default",
            status_off = "default",
            plain = TRUE,
            icon_off = icon("university"),
            icon_on = icon ("university")
          ),
          shiny::conditionalPanel(condition = paste0('input[\'', ns('showhelptextPanel2_3'), "\']"),
              shiny::uiOutput(ns('tot_info2_3'))
          )
        )
      ),
      shiny::column(12,
      fluidRow(
        shiny::column(
          6,
          div(
            style = "font-size: 25px; padding: 0px 0px; margin-left:0em",
            shiny::uiOutput(ns("ddp1_header"))
          )
        ),
        shiny::column(
          4,
          div(
            style = "font-size: 20px; padding: 0px 0px; margin-left:0em",
            shiny::uiOutput(ns("ddp2_header"))
          )
        ),
        shiny::column(
          1,
          shiny::uiOutput(ns("ddp3_header"))
        ),
        shiny::column(
          1,
          shiny::uiOutput(ns("ddp4_header"))
        )
      ),
      fluidRow(
        shiny::column(
          6,
          shinycssloaders::withSpinner(
            shiny::uiOutput(ns("ddp1")),
            type = 4
          )
        ),
        shiny::column(
          4,
          shiny::uiOutput(ns("ddp2"))
        ),
        shiny::column(
          1,
          div(
            style = "font-size: 20px; padding: 0px 0px;margin-top: 0em; margin-right:0em; margin-left:-1.1em",
            shiny::uiOutput(ns("ddp3"))
          )
        ),
        shiny::column(
          1,
          div(
            style = "font-size: 20px; padding: 0px 0px;margin-top: 0em; margin-right:0em; margin-left:-1.1em",
            shiny::uiOutput(ns("ddp4"))
          )
        )
      )
    )
  )
  )
}

#' Reactive values for mod_graph_ui Function module
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @return list with following components
#' \describe{
#'   \item{go_tab2}{go_tab2}
#' }
#' @export
mod_graph_ui_vars <- function(input, output, session) {
  
  return(
    list(
      go_tab2 = shiny::reactive({
        input$go_tab2}),
      SI_var2 = shiny::reactive({
        input$SI_var2}),
      switch2 = shiny::reactive({
        input$switch2})
    )
  )
}

#' graph Server Function
#'
#' @noRd
mod_graph_server <- function(input, output, session, mod_dataUpload_server_vars, 
                             go_tab1, go_tab3, SI_var, SI_disp, SI_order_by, SI_adjust, 
                             alpha, SI_filter, SI_strat, SI_fisher_alternative, calcDDP, 
                             min.event, max) {
  ns <- session$ns

  output$error_message_adae <- shiny::renderUI({
    if (mod_dataUpload_server_vars$error_message_flag_adae()) {
      HTML(
        paste0(
          "<span style='color: #EE2C2C; font-size: 150%;'> ", mod_dataUpload_server_vars$error_message_adae() ," </span>"
        )
      )
    }
  })
  
  output$mode_flag <- shiny::reactive({mod_dataUpload_server_vars$mode()})
  shiny::outputOptions(output, "mode_flag", suspendWhenHidden = FALSE)
  
  output$SI_var <- shiny::renderUI({
    shiny::req(mod_dataUpload_server_vars$meddraVersion())
    if (mod_dataUpload_server_vars$meddraVersion() != "NoMeddra") {
      shiny::selectInput(ns("SI_var2"),
        label = HTML("<span style = 'color: white;'> Safety variable </span>"),
        choices = list(
          "System Organ Class" = "AEBODSYS",
          "Preferred term" = "AEDECOD",
          "Medical labeling grouping" = "MLG_label"
        ),
        selected = "AEBODSYS"
      )
    } else {
      shiny::selectInput(ns("SI_var2"),
        label = HTML("<span style = 'color: white;'> Safety variable </span>"),
        choices = list(
          "System Organ Class" = "AEBODSYS",
          "Preferred term" = "AEDECOD"
        ),
        selected = "AEBODSYS"
      )
    }
  })

  shiny::observeEvent(input$SI_var2, {
    SI_var$val <- input$SI_var2
  })
  output$SI_disp2 <- shiny::renderUI({
    shiny::selectInput(ns("SI_disp2"),
      label = HTML("<span style = 'color: white;'> Effect </span>"),
      choices = list(
        "Relative Risk" = "RR",
        "Risk Difference" = "RD"
      ),
      selected = SI_disp$val
    )
  })

  shiny::observeEvent(input$SI_disp2, {
    SI_disp$val <- input$SI_disp2
  })
  output$SI_order_by2 <- shiny::renderUI({
    shiny::selectInput(ns("SI_order_by2"),
      label = HTML("<span style = 'color: white;'> Ordering by </span>"),
      choices = list(
        "p-value" = "p-value",
        "Effect" = "effect"
      ),
      selected = SI_order_by$val
    )
  })

  shiny::observeEvent(input$SI_order_by2, {
    SI_order_by$val <- input$SI_order_by2
  })
  output$SI_adjust <- shiny::renderUI({
    shiny::selectInput(ns("SI_adjust2"),
      label = HTML("<span style = 'color: white;'> p-value adjustment </span>"),
      choices = list(
        "False Discovery Rate" = "FDR",
        "New Double False Discovery Rate" = "DFDR" 
      ),
      selected = SI_adjust$val
    )
  })

  shiny::observeEvent(input$go_tab2, {
    SI_adjust$val <- input$SI_adjust2
  })

  output$SI_strat2 <- shiny::renderUI({
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
      inputId = ns("SI_strat2"), 
      label = HTML("<span style = 'color: white;'> Stratify by </span>"),
      selected = SI_strat$val,
      choices = choices
    )
  })

  shiny::observeEvent(input$SI_strat2, {
    SI_strat$val <- input$SI_strat2
  })
  nRowDdp <- shiny::reactiveValues(val = 25)
  
  output$DDP_nrows <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
      inputId = ns("DDP_nrows"), 
      label = HTML("<span style = 'color: white;'> Number AEs shown </span>"),
      choices = c("25","50","100","1000"),
      selected = "25",
      fill = TRUE,
      status = "info"
    )
  })

  shiny::observeEvent(c(calcDDP(),input$DDP_nrows), {
    shiny::req(calcDDP())
    if(input$switch2 == TRUE){
      if(length(input$DDP_nrows)>0){
        nRowDdp$val <- as.numeric(input$DDP_nrows)
      }
    }
  })
  
  shiny::observeEvent(input$SI_strat2, {
    if (input$SI_strat2 != "None") {
    
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "SI_fisher_alternative2",
        choices = list("Two sided" = "two.sided"),
        selected = "two.sided"
      )
    } else {
      shinyWidgets::updatePrettyRadioButtons(
        session,
        inputId = "SI_fisher_alternative2",
        choices = list(
          "Two sided" = "two.sided",
          "One sided" = "less"
        ),
        selected = SI_fisher_alternative$val
      )
    }
  })
  output$SI_fisher_alternative <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(
      inputId = ns("SI_fisher_alternative2"),
      label = HTML("<span style = 'color: white;'> Alternative for Fisher Test </span>"),
      choices = list(
        "Two sided" = "two.sided",
        "One sided" = "less"
      ),
      selected = "two.sided",
      fill = TRUE, status = "info"
    )
  })

  shiny::observeEvent(input$SI_fisher_alternative2, {
    SI_fisher_alternative$val <- input$SI_fisher_alternative2
  })

  output$SI_filter2 <- shiny::renderUI({
    shiny::req(min.event())
    minE <- min.event()
    minEv <- paste0("Use the minimum number of events in Verum (n = ", minE, ")")
    choices <- list("nomethod", "onepercent", "min")
    names(choices) <- c("No Method", "Overall incidence >= 1%", minEv)
    shinyWidgets::prettyRadioButtons(ns("SI_filter2"),
      label = HTML("<span style = 'color: white;'> Choose Filter Method </span>"),
      choices = choices,
      selected = SI_filter$val,
      fill = TRUE, status = "info"
    )
  })
  shiny::observeEvent(input$SI_filter2, {
    SI_filter$val <- input$SI_filter2
  })
  output$SI_alpha2 <- shiny::renderUI({
    shinyWidgets::prettyRadioButtons(ns("SI_alpha2"),
      label = HTML("<span style = 'color: white;'> Significance level (alpha) </span>"),
      choices = c("0.01", "0.05", "0.1"),
      selected = alpha$val,
      fill = TRUE,
      status = "info"
    )
  })

  shiny::observeEvent(c(input$SI_alpha2), {
    alpha$val <- input$SI_alpha2
  })
  
  output$myImage2 <- shiny::renderUI({
    list(shiny::HTML("<img src = 'AppIcon_BAG_DetectoR_210x210mm_RGB.png' alt = 'Graphic cannot be displayed' width = '400' height = '400'>"))
  })
  output$tot_info2 <- shiny::renderUI({
    shiny::req(mod_dataUpload_server_vars$adsl_filtered(), mod_dataUpload_server_vars$adae_filtered())
    HTML(paste0(
      "<b style = 'color: white;'>", "Total number ", "of subjects (ADSL): ", dim(mod_dataUpload_server_vars$adsl_filtered())[1], "</b>",
      "<b style = 'color: #EE2C2C;'>", "&nbsp; &nbsp; &nbsp; Verum (N = ", dim(mod_dataUpload_server_vars$adsl_filtered() %>%
        dplyr::filter(TRTA_DetectoR == "Verum"))[1], ")", "</b>",
      "<b style = 'color: #1e90ff;'>", "&nbsp; &nbsp; &nbsp; Comparator (N = ", dim(mod_dataUpload_server_vars$adsl_filtered() %>%
        dplyr::filter(TRTA_DetectoR == "Comparator"))[1], ")", "</b>",
      "<b style = 'color: white;'>", "&nbsp; &nbsp; &nbsp; Total AE lines (ADAE): ", dim(mod_dataUpload_server_vars$adae_filtered())[1], "</b>"
    ))
  })

  output$ddp1_header <- shiny::renderUI({
    shiny::HTML("<p style = 'color: white'> Double Dot Plot
      </p>")
  })


  shiny::observeEvent(c(go_tab1(), input$go_tab2, mod_dataUpload_server_vars$apply(), mod_dataUpload_server_vars$apply2()), {
    output$ddp2_header <- shiny::renderUI({
      shiny::req(shiny::isolate(alpha$val), shiny::isolate(SI_disp$val))
      if (shiny::isolate(SI_disp$val) == "RR") {
        text <- "Relative Risk"
      } else {
        text <- "Risk difference"
      }
      shiny::HTML(paste0("<p style = 'color: white'> ", text, " with ", shiny::isolate(100 * (1 - as.numeric(alpha$val))), "% CI</p>"))
    })
  })

  shiny::observeEvent(c(go_tab1(), input$go_tab2, mod_dataUpload_server_vars$apply(), mod_dataUpload_server_vars$apply2()), {
    output$ddp3_header <- shiny::renderUI({
      shiny::req(SI_adjust$val)
      shiny::HTML(paste0("<p style = 'color: white'> ", shiny::isolate(SI_adjust$val), " adjusted </p>"))
    })
  })
  shiny::observeEvent(c(go_tab1(), input$go_tab2, mod_dataUpload_server_vars$apply(), mod_dataUpload_server_vars$apply2()), {
    output$ddp4_header <- shiny::renderUI({
      if (shiny::isolate(SI_strat$val) == TRUE) {
        text <- "Study-size adjusted p-values"
      } else {
        text <- "Fishers exact p-values"
      }
      shiny::HTML(paste0("<p style = 'color: white'> ", text, " </p>"))
    })
  })
  
  shiny::observeEvent(c(go_tab1(), input$go_tab2, go_tab3(), mod_dataUpload_server_vars$apply(), mod_dataUpload_server_vars$apply2()), {

    output$ddp_summary_1 <- renderPlot({
      if(!is.null(calcDDP())){
        plotInfo <- calcDDP()
        variable <- shiny::isolate(SI_var$val)
        display2 <- shiny::isolate(SI_disp$val)
        adjustment <- shiny::isolate(SI_adjust$val)
        order_by <- shiny::isolate(SI_order_by$val)
        study_strat <- shiny::isolate(SI_strat$val)
        measure <- shiny::isolate(mod_dataUpload_server_vars$measure())
        numberAE <- shiny::isolate(nRowDdp$val)
        
        numberAEs <- numberAE * 2
        
        if (adjustment == "DFDR" &
            variable != "AEBODSYS" &
            order_by == "p-value") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(DFDR) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "FDR" &
                   variable != "AEBODSYS" &
                   order_by == "p-value") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(p_adj.value) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "DFDR" &
                   variable != "AEBODSYS" &
                   order_by == "effect") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "FDR" &
                   variable != "AEBODSYS" &
                   order_by == "effect") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "DFDR" &
                   variable == "AEBODSYS" &
                   order_by == "p-value") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(p) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "FDR" &
                   variable == "AEBODSYS" &
                   order_by == "p-value") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(p_adj.value) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "DFDR" &
                   variable == "AEBODSYS" &
                   order_by == "effect") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
              .[1:numberAEs, ]
          }
        } else if (adjustment == "FDR" &
                   variable == "AEBODSYS" &
                   order_by == "effect") {
          if (dim(plotInfo)[1] >= numberAEs) {
            plotInfo <- plotInfo %>%
              dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
              .[1:numberAEs, ]
          }
        }
        plotInfo <- plotInfo %>%
          dplyr::mutate(PT_collapsed = PT %>%
                          stringr::str_sub(1, 20) %>%
                          stringr::str_c("...") %>%
                          factor(levels = PT %>%
                                   stringr::str_sub(1, 20) %>%
                                   stringr::str_c("...") %>%
                                   unique() %>%
                                   rev()))
        
        label <- ifelse(input$showLabel %% 2 == 0, "PT_collapsed", "PT")
        
        double_dot_plot_p1(
          data = plotInfo,
          adjustment = adjustment,
          variable = variable,
          display = display2,
          numberAEs = numberAE,
          label = label,
          measure = measure
        )
      } else {
        shinyalert("Nothing to display...", type = "error")
        empty_plot("Nothing to display")
      }
      },
      height = function() {
        150 + (nRowDdp$val * 20)
      }
    )

    output$ddp1 <- shiny::renderUI({
      div(
        shiny::plotOutput(ns("ddp_summary_1"),
          hover = hoverOpts(ns("plot_hover1"), delay = 300, delayType = "debounce"),
          height = 'auto'
        ),
        shiny::uiOutput(ns("hover_info1"))
      )
    })
  })
  
  output$ddp_summary_2 <- shiny::renderPlot(
    {
      shiny::req(calcDDP())
      mod_dataUpload_server_vars$apply()
      mod_dataUpload_server_vars$apply2()
      go_tab1()
      input$go_tab2

      plotInfo <- calcDDP()
      
      variable <- shiny::isolate(SI_var$val)
      display2 <- shiny::isolate(SI_disp$val)
      adjustment <- shiny::isolate(SI_adjust$val)
      order_by <- shiny::isolate(SI_order_by$val)
      study_strat <- shiny::isolate(SI_strat$val)
      numberAE <- nRowDdp$val

      alph <- 100 * (1 - as.numeric(shiny::isolate(alpha$val)))
      numberAEs <- numberAE * 2
      #

      if (adjustment == "DFDR" &
        variable != "AEBODSYS" &
        order_by == "p-value") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(DFDR) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "FDR" &
        variable != "AEBODSYS" &
        order_by == "p-value") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(p_adj.value) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "DFDR" &
        variable != "AEBODSYS" &
        order_by == "effect") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "FDR" &
        variable != "AEBODSYS" &
        order_by == "effect") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "DFDR" &
        variable == "AEBODSYS" &
        order_by == "p-value") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(p) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "FDR" &
        variable == "AEBODSYS" &
        order_by == "p-value") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(p_adj.value) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "DFDR" &
        variable == "AEBODSYS" &
        order_by == "effect") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
            .[1:numberAEs, ]
        }
      } else if (adjustment == "FDR" &
        variable == "AEBODSYS" &
        order_by == "effect") {
        if (dim(plotInfo)[1] >= numberAEs) {
          plotInfo <- plotInfo %>%
            dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
            .[1:numberAEs, ]
        }
      }

      plotInfo$PT <- factor(plotInfo$PT)

      double_dot_plot_p2(
        data = plotInfo,
        adjustment = adjustment,
        variable = variable,
        display = display2,
        numberAEs = numberAEs,
        alpha = alph
      )
    },
    height = function() {
      150 + (nRowDdp$val * 20)
    }
  )
  shiny::observeEvent(c(go_tab1(), input$go_tab2), {
    output$ddp2 <- shiny::renderUI({
      mod_dataUpload_server_vars$apply()
      mod_dataUpload_server_vars$apply2()
      go_tab1()
      input$go_tab2
      div(
        shiny::plotOutput(ns("ddp_summary_2"), height = "auto", hover = hoverOpts(ns("plot_hover2"), delay = 300, delayType = "debounce")),
        shiny::uiOutput(ns("hover_info2"))
      )
    })
  })

  
  output$hover_info1 <- shiny::renderUI({
    shiny::req(calcDDP(), nRowDdp$val, input$plot_hover1)
    plotInfo <- calcDDP()
    numberAE <- nRowDdp$val
    
    numberAEs <- numberAE * 2
   
    display2 <- shiny::isolate(SI_disp$val)
    variable <- shiny::isolate(SI_var$val)
    adjustment <- shiny::isolate(SI_adjust$val)
    order_by <- shiny::isolate(SI_order_by$val)
    
    if (order_by == "p-value") {
      if (adjustment == "DFDR") {
        if (!is.null(variable) & variable == "AEBODSYS") {
          var1 = "p"
          var2 <- "rr.missing_label"
        } else if (!is.null(variable) & variable == "AEDECOD") {
          var1 <- "p"
          var2 <- "DFDR"
        } else if (!is.null(variable) & variable == "MLG_label") {
          var1 <- "p"
          var2 <- "DFDR"
        }
      } else {
        var1 <- "p"
        var2 <- "p_adj"
      }
      plotInfo <- plotInfo %>% 
        dplyr::arrange(!!rlang::sym(var2), !!rlang::sym(var1))
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          .[1:numberAEs,]
      }
    } else if (order_by == "effect") {
      plotInfo <- plotInfo %>%
        dplyr::arrange(desc(!!rlang::sym(tolower(display2))))
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          .[1:numberAEs,]
      }
    }
    
    plotInfo <- plotInfo %>%
      dplyr::mutate(
        PT_collapsed = PT %>%
        stringr::str_sub(1, 20) %>%
        stringr::str_c("...") %>%
        factor(
          levels = PT %>%
          stringr::str_sub(1,20) %>%
          stringr::str_c("...") %>%
          unique() %>%
          rev()
        )
      )
      
    hover <- input$plot_hover1
    
    display <- SI_disp$val
    
    point <- nearPoints(plotInfo, hover) %>%
      dplyr::arrange(TRTA_DetectoR)
    
    if (nrow(point) == 0) return(NULL)
    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
    
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    left_px <-  (hover$range$left + left_pct * (hover$range$right - hover$range$left) /
                   hover$img_css_ratio$x) - 175
    top_px <- 20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    if (dim(point)[1] == 2) {
      col_tmp <- "rgba(129, 96, 154, 0.85)"
      prob_text <- c(point$prob1[1],point$prob2[1])
    } else if (dim(point)[1] == 1) {
      if (point$TRTA_DetectoR == "Verum") {
        col_tmp <- "rgba(30,144,255,0.85)"
        prob_text <- point$prob2
      } else if (point$TRTA_DetectoR == "Comparator") {
        col_tmp <- "rgba(238,44,44,0.85)"
        prob_text <- point$prob1
      }
    }
    
    style <- paste0("position:absolute; z-index:100; background-color: ", col_tmp,"; ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")


    shiny::wellPanel(
     style = style,
      p(
        HTML(
          paste(
            "<b style = 'color: white'>", point$TRTA_DetectoR,": ", 
            ifelse(rep(shiny::isolate(mod_dataUpload_server_vars$measure()) == "proportions",length(prob_text)),
              round(100*prob_text,2),
              round(prob_text,2)
            ),
            " (N = ",
            point %>% dplyr::pull(count),")"
          )
        )
      )
    )
  })
  
  output$hover_info2 <- renderUI({
    req(calcDDP(), nRowDdp$val, input$plot_hover2)
    input$plot_hover2
    input$ddp_summary_2
    go_tab1()
    input$go_tab2

    plotInfo <- calcDDP()

    numberAE <- nRowDdp$val

    numberAEs <- numberAE * 2
    display2 <- shiny::isolate(SI_disp$val)
    variable <- shiny::isolate(SI_var$val)
    adjustment <- shiny::isolate(SI_adjust$val)
    order_by <- shiny::isolate(SI_order_by$val)
    if (adjustment == "DFDR" &
      variable != "AEBODSYS" &
      order_by == "p-value") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(DFDR) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "FDR" &
      variable != "AEBODSYS" &
      order_by == "p-value") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(p_adj.value) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "DFDR" &
      variable != "AEBODSYS" &
      order_by == "effect") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "FDR" &
      variable != "AEBODSYS" &
      order_by == "effect") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "DFDR" &
      variable == "AEBODSYS" &
      order_by == "p-value") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(p) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "FDR" &
      variable == "AEBODSYS" &
      order_by == "p-value") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(p_adj.value) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "DFDR" &
      variable == "AEBODSYS" &
      order_by == "effect") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
          .[1:numberAEs, ]
      }
    } else if (adjustment == "FDR" &
      variable == "AEBODSYS" &
      order_by == "effect") {
      if (dim(plotInfo)[1] >= numberAEs) {
        plotInfo <- plotInfo %>%
          dplyr::arrange(desc(!!rlang::sym(tolower(display2)))) %>%
          .[1:numberAEs, ]
      }
    }
    plotInfo$rd <- plotInfo$rd * 100
    plotInfo$rd.lcl <- plotInfo$rd.lcl * 100
    plotInfo$rd.ucl <- plotInfo$rd.ucl * 100

    plotInfo$PT <- factor(plotInfo$PT)

    hover <- input$plot_hover2

    display <- SI_disp$val

    point <- nearPoints(plotInfo, hover)

    if (nrow(point) == 0) {
      return(NULL)
    }

    if (display == "RD") {
      left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)

      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) /
        hover$img_css_ratio$x
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    } else if (display == "RR") {
      left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left) /
        hover$img_css_ratio$x
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top) /
        hover$img_css_ratio$y
    }

    style <- paste0(
      "position:absolute; z-index:100; background-color: rgba(82, 82, 82, 0.85); ",
      "left:", left_px, "px; top:", top_px, "px; border: 0px;"
    )

    wellPanel(
      style = style,
      p(HTML(paste0(
        "<b style = 'color: white'> Lower limit: ", point %>% dplyr::pull(!!rlang::sym(paste0(tolower(display), ".lcl"))) %>% unique() %>% round(2), "<br/>",
        "<b style = 'color: white'> ", ifelse(display == "RD", "Risk Difference: ", "Relative Risk: "), point %>% dplyr::pull(!!rlang::sym(paste0(tolower(display)))) %>% unique() %>% round(2), "<br/>",
        "<b style = 'color: white'> Upper limit: ", point %>% dplyr::pull(!!rlang::sym(paste0(tolower(display), ".ucl"))) %>% unique() %>% round(2), "<br/>"
      )))
    )
  })

  output$ddp_summary_3 <- renderPlot(
    {
      shiny::req(calcDDP())

      plotInfo <- calcDDP()
      variable <- shiny::isolate(SI_var$val)
      display2 <- shiny::isolate(SI_disp$val)
      adjustment <- shiny::isolate(SI_adjust$val)
      order_by <- shiny::isolate(SI_order_by$val)
      study_strat <- shiny::isolate(SI_strat$val)
      numberAE <- nRowDdp$val

      double_dot_plot_p3(
        data = plotInfo, 
        adjustment = adjustment, 
        variable = variable, 
        display = display2, 
        #study_strat = study_strat,
        numberAEs = numberAE,
        order_by = order_by
      )
    },
    height = function() {
      150 + (nRowDdp$val * 20)
    }
  )

  output$ddp3 <- shiny::renderUI({
    shiny::plotOutput(ns("ddp_summary_3"), height = "auto")
  })

  output$ddp_summary_4 <- renderPlot(
    {
      shiny::req(calcDDP())

      plotInfo <- calcDDP()
      variable <- shiny::isolate(SI_var$val)
      display2 <- shiny::isolate(SI_disp$val)
      adjustment <- shiny::isolate(SI_adjust$val)
      order <- shiny::isolate(SI_order_by$val)
      study_strat <- shiny::isolate(SI_strat$val)
      numberAE <- nRowDdp$val

      double_dot_plot_p4(
        data = plotInfo,
        adjustment = adjustment,
        variable = variable,
        display = display2,
        study_strat = study_strat,
        numberAEs = numberAE,
        order_by = order
      )
    },
    height = function() {
      150 + (nRowDdp$val * 20)
    }
  )

  output$ddp4 <- shiny::renderUI({
    shiny::plotOutput(ns("ddp_summary_4"), height = "auto")
  })
  
  
  output$tot_info2_1 <- shiny::renderUI({
    shiny::req(tot_info_text2_1())
    HTML(tot_info_text2_1())
  })

  tot_info_text2_1 <- shiny::reactive({
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

    ### Total Subjects 
    total <- comb_data %>% 
      dplyr::select(USUBJID) %>% 
      tidyr::drop_na() %>% 
      dplyr::pull()%>% 
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
    
    ## Subject with Adverse Events

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
      
      ## Subject with Adverse Events
  
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
  
  
  output$tot_info2_3 <- shiny::renderUI({
    shiny::req(tot_info_text2_3())
    HTML(tot_info_text2_3())
  })

  tot_info_text2_3 <- shiny::reactive({
    # number of STUDYIDs
    number_studies <- length(unique(mod_dataUpload_server_vars$adsl_filtered()$STUDYID))
    # table of subjects in studies
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
  
  return(list(
    go_tab2 = shiny::reactive({
      input$go_tab2
    })
  ))
}

#' create an empty plot
#'
#'@param label A character string with the label
#'
#'

empty_plot = function(label){
  y <- x <- NULL
  data.frame(x = 0.5, y = 0.5, label = label) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_text(ggplot2::aes(label = label)) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::theme(axis.line=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_blank(),
          axis.text.y=ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          axis.title.x=ggplot2::element_blank(),
          axis.title.y=ggplot2::element_blank(),
          legend.position="none",
          panel.background=ggplot2::element_blank(),
          panel.border=ggplot2::element_blank(),
          panel.grid.major=ggplot2::element_blank(),
          panel.grid.minor=ggplot2::element_blank(),
          plot.background=ggplot2::element_blank())
}

## To be copied in the UI
# mod_graph_ui("graph_ui_1")

## To be copied in the server
# callModule(mod_graph_server, "graph_ui_1")
