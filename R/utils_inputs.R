#' pickerInput options by default
#'
picker_input_options <- function() {
  shinyWidgets::pickerOptions(
    actionsBox = TRUE,
    selectedTextFormat = "count",
    countSelectedText = "{0} selected (of {1})",
    liveSearch = TRUE,
    header = "Select multiple items",
    noneSelectedText = "No selection!"
  )
}

#' Shorthand for custom UI row
#' @param ... Contents of the row.
flex_row <- function(...) {
  bslib::layout_columns(
    ...,
    col_widths = "auto",
    class = "align-items-center justify-content-evenly gap-3 mt-2"
  )
}

# #' Shorthand for custom UI row - left side
# #' @param ... Contents of the row.
# row_left_side <- function(...) {
#   shiny::div(class = "upload-left", ...)
# }

# #' Shorthand for custom UI row - right side
# #' @param ... Contents of the row.
# row_right_side <- function(...) {
#   shiny::div(class = "upload-right", ...)
# }

# #' Shorthand for custom UI row - symmetrical halves
# #' @param ... Contents of the row.
# row_half <- function(...) {
#   shiny::div(class = "upload-half", ...)
# }

#' Shorthand for invisible text (triggers conditional panels, but it's not
#' visible)
#' @param ... Contents of the row.
invisible_text <- function(...) {
  shiny::div(
    style = "visibility:hidden;height:0;overflow:hidden;",
    shiny::textOutput(...)
  )
}
