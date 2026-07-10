#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for `{shiny}`.
#'
#' @noRd
mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    class = "about",
    shiny::h1("Welcome to DetectoR"),
    shiny::div(
      class = "detector-logo-about",
      alt = "DetectoR logo"
    ),
    shiny::p(
      "The DetectoR R Shiny app provides a handy platform allowing for
              early identification of signals and an ongoing monitoring of
              safety along the medical product development phase and
              lifecycle."
    ),
    shiny::h2("Help and documentation"),
    shiny::p(
      "You can find information about the data requirements in the 'Data
             Manual' tab on the sidebar."
    ),
    shiny::p(
      "For more information about the app, please check the 'About'
             tab."
    ),
    shiny::h2("Get Started"),
    shiny::actionButton(
      ns("next_upload"),
      "Upload Data",
      icon = shiny::icon("upload"),
      class = "btn-lg align-center"
    )
  )
}

#' welcome Server Functions
#'
#' @noRd
mod_welcome_server <- function(id, r) {
  shiny::moduleServer(id, function(input, output, session) {
    # Move to "upload" tab
    shiny::observeEvent(input$next_upload, {
      shinydashboard::updateTabItems(
        session = r$parent_session,
        inputId = "tabs",
        selected = "upload"
      )
    })
  })
}
