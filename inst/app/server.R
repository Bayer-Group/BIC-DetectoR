function(input, output, session) {
  ns <- session$ns
  
  # Time Count Feature
  tmp_time <- Sys.time()
  print(paste0("Started at ", tmp_time))
  session$onSessionEnded(function() {
    print(paste0("Ended at ", Sys.time()))
    print("Time used:")
    print(round(Sys.time() - tmp_time), 2)
  })

  # Increase UploadSize Limit
  options(shiny.maxRequestSize = 700 * 1024^2)

  # call data upload module
  mod_dataUpload_server_vars <- shiny::callModule(mod_dataUpload_server, "dataUpload_ui_1")
  
  # main panel module
  mod_mainPanel_server_vars <- shiny::callModule(mod_mainPanel_server, "mainPanel_ui_1", mod_dataUpload_server_vars)
  
  # report module
  shiny::callModule(mod_report_server, "report_ui_1", mod_dataUpload_server_vars, mod_mainPanel_server_vars)

  # flag
  start <- shiny::reactiveValues(dat = FALSE)
  start2 <- shiny::reactiveValues(dat = FALSE)
  
  output$flag <- shiny::reactive(start$dat)
  output$flag2 <- shiny::reactive(start2$dat)
  
  flag_upload <- shiny::reactive({
    shiny::req(
      mod_dataUpload_server_vars$adae_data(),
      mod_dataUpload_server_vars$adsl_data(), 
      mod_dataUpload_server_vars$meddraVersion()
    )
    1
  })
  
  flag_upload2 <- shiny::eventReactive(c(mod_dataUpload_server_vars$apply2(), input$retrieve_files), {
    if (!is.null(mod_dataUpload_server_vars$selectTreatment()) &
        !is.null(mod_dataUpload_server_vars$pickerinput_Verum()) & 
        !is.null(mod_dataUpload_server_vars$pickerinput_Comparator())
      ) {
      1
    }
  })
  
  shiny::observeEvent(flag_upload(), {
    start$dat <- TRUE
  })
  shiny::observeEvent(flag_upload2(), {
    start2$dat <- TRUE
  })
  shiny::outputOptions(output, "flag", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "flag2", suspendWhenHidden = FALSE)
  
  shiny::observeEvent(flag_upload(), {
    if (flag_upload() == 1) {
      shinyjs::useShinyjs()
      shinyjs::disable("dataUpload_ui_1-adsl_file")
      shinyjs::disable("dataUpload_ui_1-adae_file")
      shinyjs::disable("dataUpload_ui_1-mode")
        if (mod_dataUpload_server_vars$mode() == "demo") {
          shinyjs::delay(500, shinyjs::click("dataUpload_ui_1-apply2"))
        }
    }
  })
  
  # heatmap
  start_heat <- shiny::reactiveValues(dat = FALSE)
  output$heat_flag <- shiny::reactive(start_heat$dat)
  shiny::outputOptions(output, "heat_flag", suspendWhenHidden = FALSE)
  shiny::observeEvent(mod_mainPanel_server_vars$go_tab3(), {
    start_heat$dat <- TRUE
  })
  shiny::observeEvent(flag_upload(), {
    if (flag_upload() == 1) {
      shinyWidgets::updatePrettyToggle(
        session, 
        inputId = "sh_Upload",
        value = FALSE
      )
    }
  })

  start_error <- shiny::reactiveValues(dat = FALSE)
  output$error_message_flag <- shiny::reactive({start_error$dat})
  
  shiny::observeEvent(mod_dataUpload_server_vars$error_message_flag_adae(), {
    start_error$dat <- mod_dataUpload_server_vars$error_message_flag_adae()
  })
  
  shiny::outputOptions(output, "error_message_flag", suspendWhenHidden = FALSE)
  
  shiny::observeEvent(mod_dataUpload_server_vars$apply2(), {
    # shinyWidgets::updatePrettyToggle(
    #   session, 
    #   inputId = "showMeasure",
    #   value = FALSE
    # )
    shinyWidgets::updatePrettyToggle(
      session, 
      inputId = "showSelectTreatment",
      value = FALSE
    )
    shinyWidgets::updatePrettyToggle(
      session, 
      inputId = "sh_Upload",
      value = FALSE
    )
    },ignoreInit = TRUE
  )
  
  # Advanced settings
  condition_filter <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(mod_dataUpload_server_vars$condition_filter(), {
    condition_filter$val <- mod_dataUpload_server_vars$condition_filter()
  })
  output$condition_filter <- shiny::reactive(condition_filter$val)
  shiny::outputOptions(output,"condition_filter", suspendWhenHidden = FALSE)
  
  advanced <- shiny::reactiveValues(val = FALSE)
  output$advanced <- shiny::reactive(advanced$val)
  shiny::outputOptions(output,"advanced", suspendWhenHidden = FALSE)
  shiny::observeEvent(mod_mainPanel_server_vars$switch(), {
    advanced$val <- mod_mainPanel_server_vars$switch()
  })
  advanced2 <- shiny::reactiveValues(val = FALSE)
  output$advanced2 <- shiny::reactive(advanced2$val)
  shiny::outputOptions(output,"advanced2", suspendWhenHidden = FALSE)
  shiny::observeEvent(mod_mainPanel_server_vars$switch2(),{
    advanced2$val <- mod_mainPanel_server_vars$switch2()
  })
  advanced3 <- shiny::reactiveValues(val = FALSE)
  output$advanced3 <- shiny::reactive(advanced3$val)
  shiny::outputOptions(output,"advanced3", suspendWhenHidden = FALSE)
  shiny::observeEvent(mod_mainPanel_server_vars$switch3(),{
    advanced3$val <- mod_mainPanel_server_vars$switch3()
  })
}
