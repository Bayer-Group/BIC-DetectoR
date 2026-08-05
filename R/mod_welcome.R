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
    class = "about border-0 shadow-sm mx-auto",
    shiny::h1("Welcome to DetectoR"),
    shiny::img(
      src = "www/logos/AppIcon_BAG_DetectoR_210x210mm_RGB.png",
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
      class = "btn-lg d-block mx-auto btn-primary"
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
      bslib::nav_select(
        "tabs",
        selected = "upload",
        session = r$parent_session
      )
    })
  })
}
