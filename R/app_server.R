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
  output$filter <- shinydashboard::renderMenu({
    shiny::req(r$unfiltered_data)
    shinydashboard::menuItem(
      "Filter data",
      tabName = "filter",
      icon = shiny::icon("filter")
    )
  })
  output$graph <- shinydashboard::renderMenu({
    shiny::req(r$filtered_data)
    shinydashboard::menuItem(
      "Double Dot Plot",
      tabName = "graph",
      icon = shiny::icon("list-alt")
    )
  })
  output$heatmap <- shinydashboard::renderMenu({
    shiny::req(r$filtered_data)
    shinydashboard::menuItem(
      "Heatmap",
      tabName = "heatmap",
      icon = shiny::icon("th")
    )
  })
  output$volcano <- shinydashboard::renderMenu({
    shiny::req(r$filtered_data)
    shinydashboard::menuItem(
      "Volcano",
      tabName = "volcano",
      icon = shiny::icon("volcano")
    )
  })
  output$table <- shinydashboard::renderMenu({
    shiny::req(r$filtered_data)
    shinydashboard::menuItem(
      "View dataset",
      tabName = "table",
      icon = shiny::icon("table")
    )
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
