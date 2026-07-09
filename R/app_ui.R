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
      title = shiny::span(
        shiny::div(class = "detector-logo-sidebar"),
        "DetectoR"
      ),
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
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = "30%",
            bslib::accordion(
              mod_info_ui("info_upload")
            )
          ),
          mod_upload_ui("upload_1")
        )
      ),
      bslib::nav_panel(
        "Double Dot Plot",
        value = "graph",
        icon = shiny::icon("list-alt"),
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            width = "30%",
            bslib::accordion(
              mod_filter_ui("filter_1"),
              mod_info_ui("info_filter")
            )
          ),
          mod_graph_ui("graph_1")
        )
      ),
      bslib::nav_panel(
        "Heatmap",
        value = "heatmap",
        icon = shiny::icon("th"),
        mod_heatmap_ui("heatmap_1")
      ),
      bslib::nav_panel(
        "Volcano Plot",
        value = "volcano",
        icon = shiny::icon("volcano"),
        mod_volcano_ui("volcano_1")
      ),
      bslib::nav_panel(
        "Table",
        value = "table",
        icon = shiny::icon("table"),
        mod_table_ui("table_1")
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
        shiny::div(
          id = "toggle-theme-container",
          detector_pretty_toggle(
            id = "toggle_theme",
            label = "Dark Theme",
            label_off = "Light Theme",
            icon = "moon",
            icon_off = "sun"
          )
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
    # Google Noto Sans font (fallback font by Bayer)
    shiny::HTML(
      '<link rel="preconnect" href="https://fonts.googleapis.com">
      <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
      <link href="https://fonts.googleapis.com/css2?family=Noto+Sans:ital,wght@0,100..900;1,100..900&display=swap" rel="stylesheet">'
    ),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "DetectoR"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
