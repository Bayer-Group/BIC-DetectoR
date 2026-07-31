#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      id = "tabs",
      theme = detector_theme(),
      title = shiny::span(
        shiny::img(
          src = "www/logos/AppIcon_BAG_DetectoR_210x210mm_RGB.png",
          alt = "DetectoR logo",
          class = "detector-logo-sidebar"
        ),
        "DetectoR"
      ),
      # Sidebar ----
      sidebar = bslib::sidebar(
        id = "sidebar_page",
        open = FALSE,
        width = "30%",
        bslib::accordion(
          open = TRUE,
          # Plot display options
          shiny::conditionalPanel(
            "input.tabs == 'welcome'",
            "Upload data to get started!"
          ),
          # Plot display options
          shiny::conditionalPanel(
            "input.tabs == 'graph'",
            mod_calculate_ui(
              "calculate_dot",
              calculate_mode = "double_dot"
            )
          ),
          shiny::conditionalPanel(
            "input.tabs == 'heatmap'",
            mod_calculate_ui(
              "calculate_heatmap",
              calculate_mode = "heatmap"
            )
          ),
          shiny::conditionalPanel(
            "input.tabs == 'volcano'",
            mod_calculate_ui(
              "calculate_volcano",
              calculate_mode = "volcano"
            )
          ),
          shiny::conditionalPanel(
            "input.tabs == 'table'",
            mod_calculate_ui(
              "calculate_table",
              calculate_mode = "table"
            )
          ),
          # Data filters
          shiny::conditionalPanel(
            "input.tabs == 'graph' ||
              input.tabs == 'heatmap' ||
              input.tabs == 'volcano' ||
              input.tabs =='table'",
            mod_filter_ui("filter_sidebar"),
            mod_active_filters_ui("filters_sidebar")
          ),
          # Dataset info
          shiny::conditionalPanel(
            "input.tabs != 'welcome'",
            mod_info_ui("info_sidebar")
          )
        )
      ),
      # Navigation bar ----
      bslib::nav_item(
        paste0("Version: ", utils::packageVersion("DetectoR"))
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        "Welcome",
        value = "welcome",
        icon = shiny::icon("home"),
        mod_welcome_ui("welcome_1")
      ),
      bslib::nav_panel(
        "Upload",
        value = "upload",
        icon = shiny::icon("upload"),
        mod_upload_ui("upload_1")
      ),
      bslib::nav_panel(
        "Double Dot Plot",
        value = "graph",
        icon = shiny::icon("list-alt"),
        mod_graph_ui("graph_1"),
        mod_active_filters_ui("filters_dot")
      ),
      bslib::nav_panel(
        "Heatmap",
        value = "heatmap",
        icon = shiny::icon("th"),
        mod_heatmap_ui("heatmap_1"),
        mod_active_filters_ui("filters_heatmap")
      ),
      bslib::nav_panel(
        "Volcano Plot",
        value = "volcano",
        icon = shiny::icon("volcano"),
        mod_volcano_ui("volcano_1"),
        mod_active_filters_ui("filters_volcano")
      ),
      bslib::nav_panel(
        "Table",
        value = "table",
        icon = shiny::icon("table"),
        mod_table_ui("table_1"),
        mod_active_filters_ui("filters_table")
      ),
      bslib::nav_spacer(),
      bslib::nav_panel(
        "About",
        value = "help",
        icon = shiny::icon("info-circle"),
        mod_about_ui("help_1")
      ),
      bslib::nav_panel(
        "Data Manual",
        value = "manual",
        icon = shiny::icon("folder-open"),
        mod_manual_ui("manual_1")
      ),
      bslib::nav_item(
        bslib::input_dark_mode(
          id = "dark_mode",
          mode = "light",
          class = "ms-2"
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "DetectoR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
