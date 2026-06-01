#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      title = "DetectoR",
      shinydashboard::dashboardHeader(
        title = shiny::div(class = "detector-logo-header")
      ),
      shinydashboard::dashboardSidebar(
        collapsed = TRUE,
        shinydashboard::sidebarMenu(
          id = "tabs",
          shinydashboard::menuItem(
            "Welcome",
            tabName = "welcome",
            icon = shiny::icon("home")
          ),
          shinydashboard::menuItem(
            "Upload data",
            tabName = "upload",
            icon = shiny::icon("upload")
          ),
          shinydashboard::menuItemOutput("filter"),
          shinydashboard::menuItemOutput("graph"),
          shinydashboard::menuItemOutput("heatmap"),
          shinydashboard::menuItemOutput("volcano"),
          shinydashboard::menuItemOutput("table"),
          shinydashboard::menuItem(
            "About",
            tabName = "help",
            icon = shiny::icon("info-circle")
          ),
          shinydashboard::menuItem(
            "Data Manual",
            tabName = "manual",
            icon = shiny::icon("folder-open")
          ),
          shiny::div(
            id = "toggle-theme-container",
            detector_pretty_toggle(
              id = "toggle_theme",
              label = "Dark Theme",
              label_off = "Light Theme",
              icon = "moon",
              icon_off = "sun"
            )
          ),
          shiny::div(class = "detector-logo-sidebar"),
          shiny::p(
            paste0("Version: ", utils::packageVersion("DetectoR")),
            class = "align-center"
          )
        )
      ),
      shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem("welcome", mod_welcome_ui("welcome_1")),
          shinydashboard::tabItem("upload", mod_upload_ui("upload_1")),
          shinydashboard::tabItem("filter", mod_filter_ui("filter_1")),
          shinydashboard::tabItem("graph", mod_graph_ui("graph_1")),
          shinydashboard::tabItem("heatmap", mod_heatmap_ui("heatmap_1")),
          shinydashboard::tabItem("volcano", mod_volcano_ui("volcano_1")),
          shinydashboard::tabItem("table", mod_table_ui("table_1")),
          shinydashboard::tabItem("help", mod_about_ui("help_1")),
          shinydashboard::tabItem("manual", mod_manual_ui("manual_1"))
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
