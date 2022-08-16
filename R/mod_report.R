#' report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @export
#' @importFrom shiny NS tagList 
mod_report_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::wellPanel(
      style = paste0("background-color: ", col_midgrey, "; border-color: white; border: 0px; border-radius: 0px;"),
      shinyWidgets::prettyToggle(
        inputId = "showReport",
        label_off = HTML("<span style='color: white; font-size: 15px;'> Print Report </span>"),
        label_on = HTML("<span style = 'color: white;font-size: 15px; '> Print Report </span>"),
        value = FALSE,
        outline = TRUE,
        status_on = "default",
        status_off = "default",
        plain = TRUE,
        icon_off = icon("file-alt"),
        icon_on = icon("file-alt")
      ),
      shiny::conditionalPanel(
        condition = "input.showReport",
        shiny::textInput(
          inputId = ns("report_header"),
          label = "Enter a report header (optional)",
          placeholder = "Report header"
        ),
        shiny::downloadButton(
          outputId = ns("report"),
          label = "Generate report",
          class = "button_class"
        ),
        shiny::tags$head(tags$style(".button_class{color: white; background-color: #61a337}"))
      )
    )
  )
}
    
#' report Server Functions
#'
#' @noRd
#' @export
mod_report_server <- function(input, output, session, mod_dataUpload_server_vars, mod_mainPanel_server_vars){
    ns <- session$ns
    
    output$report <- shiny::downloadHandler(
      filename = function(){
        paste("detector_report", gsub(":","-",Sys.time()), ".html", sep = "")
      },
      content = function(file) {
        withProgress(message = 'Generating Report, please wait!', {
          tempReport <- file.path(tempdir(), "report.Rmd")
          file.copy("report.Rmd", tempReport, overwrite = TRUE)
          tempReport_child <- file.path(tempdir(), "AppSign_black_BAG_DetectoR_220x76mm_RGB.png")
          file.copy("www/AppSign_black_BAG_DetectoR_220x76mm_RGB.png", tempReport_child, overwrite = TRUE)
          
          if (mod_mainPanel_server_vars$SI_var$val == "AEBODSYS") {
            SI_var_text <- "System Organ Classes"
          } else if (mod_mainPanel_server_vars$SI_var$val == "AEDECOD") {
            SI_var_text <- "Preferred Terms"
          } else if (mod_mainPanel_server_vars$SI_var$val == "MLG_label") {
            SI_var_text <- "Medical Labeling Groupings"
          }
          
          if (mod_mainPanel_server_vars$SI_filter$val == "nomethod") {
            SI_filter_text <- "None" 
          } else if (mod_mainPanel_server_vars$SI_filter$val == "onepercent") {
            SI_filter_text <- "Overall icidence >= 1%"
          } else if (mod_mainPanel_server_vars$SI_filter$val == "min") {
            SI_filter_text <- paste("Minimum number of events in Verum (n = ", mod_mainPanel_server_vars$min.event(),")")
          }
          
          if (mod_dataUpload_server_vars$meddraVersion() != "NoMeddra" && mod_mainPanel_server_vars$SI_var$val == "MLG_label") {
            meddraVersion_text <- paste0(" using MedDRA version ", mod_dataUpload_server_vars$meddraVersion(), " ")
          } else {
            meddraVersion_text <- ""
          }
          
          if (mod_mainPanel_server_vars$SI_disp$val == "RR") {
            SI_disp_text <- "Relative Risks"
          } else if (mod_mainPanel_server_vars$SI_disp$val == "RD") {
            SI_disp_text <- "Risk Differences"
          }
          # Set up parameters to pass to Rmd document
          paramsList <- list(
            reportHeader = input$report_header,
            selectTreatment = mod_dataUpload_server_vars$selectTreatment(),
            meddraVersion = mod_dataUpload_server_vars$meddraVersion(),
            meddraVersion_text = meddraVersion_text,
            plotInfo = mod_mainPanel_server_vars$calcDDP(),
            SI_var = mod_mainPanel_server_vars$SI_var$val,
            SI_var_text = SI_var_text,
            SI_order_by = mod_mainPanel_server_vars$SI_order_by$val,
            SI_disp = mod_mainPanel_server_vars$SI_disp$val,
            SI_disp_text = SI_disp_text,
            SI_adjust = mod_mainPanel_server_vars$SI_adjust$val,
            SI_strat = ifelse(mod_mainPanel_server_vars$SI_strat$val!="None", 
                              paste0(mod_mainPanel_server_vars$SI_disp$val," where stratified by ", 
                                     mod_mainPanel_server_vars$SI_strat$val, "."), ""),
            SI_fisher_alternative = ifelse(mod_mainPanel_server_vars$SI_fisher_alternative$val == "two.sided","two sided","one sided"),
            SI_filter = mod_mainPanel_server_vars$SI_filter$val,
            SI_filter_text = SI_filter_text,
            SI_alpha = mod_mainPanel_server_vars$alpha$val,
            adae_file = mod_dataUpload_server_vars$adae_file()$datapath,
            adsl_file = mod_dataUpload_server_vars$adsl_file()$datapath,
            pickerinput_Verum = mod_dataUpload_server_vars$pickerinput_Verum(),
            pickerinput_Comparator = mod_dataUpload_server_vars$pickerinput_Comparator(),
            pickerinput_adae = mod_dataUpload_server_vars$pickerinput_adae(),
            pickerinput_adsl = mod_dataUpload_server_vars$pickerinput_adsl(),
            filter_text = ifelse(is.null(mod_dataUpload_server_vars$filter_list$val), 
                                 "No filter selected", paste(unlist(mod_dataUpload_server_vars$filter_list$val))),
            rendered_by_shiny = TRUE
          )
          rmarkdown::render(
            tempReport,
            output_file = file,
            params = paramsList,
            envir = new.env(parent = globalenv())
          )
        })
      }
    )
}
    
## To be copied in the UI
# mod_report_ui("report_ui_1")
    
## To be copied in the server
# mod_report_server("report_ui_1")
