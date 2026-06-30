#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  # Increase UploadSize Limit
  options(shiny.maxRequestSize = 700 * 1024^2)

  # Time Count Feature
  tmp_time <- Sys.time()
  print(paste0("Started at ", tmp_time))
  session$onSessionEnded(function() {
    print(paste0("Ended at ", Sys.time()))
    print("Time used:")
    print(round(Sys.time() - tmp_time, 2))
  })

  # Light/dark theme
  theme <- shiny::reactive({
    if (input$toggle_theme) {
      "light"
    } else {
      "dark"
    }
  })

  # Communication between modules
  r <- shiny::reactiveValues()
  r$parent_session <- session
  shiny::observe({
    r$theme <- theme()
  })

  # Conditionally show sidebar menu items ----
  shiny::observeEvent(r$unfiltered_data, {
    shiny::req(r$unfiltered_data)
    bslib::nav_show("tabs", "filter")
  })
  shiny::observeEvent(r$filtered_data, {
    shiny::req(r$filtered_data)
    bslib::nav_show("tabs", "graph")
    bslib::nav_show("tabs", "heatmap")
    bslib::nav_show("tabs", "volcano")
    bslib::nav_show("tabs", "table")
  })

  # Call servers ----
  mod_welcome_server("welcome_1", r = r)
  mod_upload_server("upload_1", r = r)
  mod_info_server("info_upload", r = r)
  mod_info_server("info_filter", r = r)
  mod_filter_server("filter_1", r = r)
  mod_calculate_server(
    "calculate_double_dot",
    r = r,
    calculate_mode = "double_dot"
  )
  mod_calculate_server("calculate_heatmap", r = r, calculate_mode = "heatmap")
  mod_calculate_server("calculate_volcano", r = r, calculate_mode = "volcano")
  mod_calculate_server("calculate_table", r = r, calculate_mode = "table")
  mod_graph_server("graph_1", r = r)
  mod_heatmap_server("heatmap_1", r = r)
  mod_volcano_server("volcano_1", r = r)
  mod_table_server("table_1", r = r)
  mod_about_server("help_1")
  mod_manual_server("manual_1")
}
