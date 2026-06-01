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

#' Default shinyWidgets::prettyToggle()
#' @param id A character, input's id.
#' @param label A character, label to show on input.
#' @param label_off (Optional) A character, label to show on input when off.
#' @param icon A character, an icon to be passed to shiny::icon().
#' @param icon_off (Optional) A character, an icon to be passed to shiny::icon()
#'  when off.
#' @param ... Optional additional parameters to prettyToggle().
detector_pretty_toggle <- function(
  id,
  label,
  label_off = NULL,
  icon,
  icon_off = NULL,
  ...
) {
  if (is.null(label_off)) {
    label_off <- label
  }
  if (is.null(icon_off)) {
    icon_off <- icon
  }
  shinyWidgets::prettyToggle(
    id,
    label_on = label,
    label_off = label_off,
    value = TRUE,
    outline = TRUE,
    status_on = "default",
    status_off = "default",
    plain = TRUE,
    icon_on = shiny::icon(icon),
    icon_off = shiny::icon(icon_off),
    ...
  )
}

#' Default prettyRadioButtons
#' @param input_id Input ID.
#' @param label Text to show in the input.
#' @param ... Options to pass to shinyWidgets::prettyRadioButtons
#'
detector_radio_buttons <- function(input_id, label, ...) {
  shinyWidgets::prettyRadioButtons(
    inputId = input_id,
    label = label,
    fill = TRUE,
    status = "info",
    ...
  )
}


#' Shorthand for custom UI row
#' @param ... Contents of the row.
flex_row <- function(...) {
  shiny::div(class = "flex-row", ...)
}

#' Shorthand for custom UI row - left side
#' @param ... Contents of the row.
row_left_side <- function(...) {
  shiny::div(class = "upload-left", ...)
}

#' Shorthand for custom UI row - right side
#' @param ... Contents of the row.
row_right_side <- function(...) {
  shiny::div(class = "upload-right", ...)
}

#' Shorthand for custom UI row - symmetrical halves
#' @param ... Contents of the row.
row_half <- function(...) {
  shiny::div(class = "upload-half", ...)
}


#' Shorthand for invisible text (triggers conditional panels, but it's not
#' visible)
#' @param ... Contents of the row.
invisible_text <- function(...) {
  shiny::div(class = "invisible-text", shiny::textOutput(...))
}
