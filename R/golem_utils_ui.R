#' Columns wrappers
#'
#' These are convenient wrappers around
#' `shiny::column(12, ...)`, `shiny::column(6, ...)`, `shiny::column(4, ...)`...
#'
#' @noRd
#'
col_12 <- function(...) {
  shiny::column(12, ...)
}

col_10 <- function(...) {
  shiny::column(10, ...)
}

col_8 <- function(...) {
  shiny::column(8, ...)
}

col_7 <- function(...) {
  shiny::column(7, ...)
}

col_6 <- function(...) {
  shiny::column(6, ...)
}

col_5 <- function(...) {
  shiny::column(5, ...)
}

col_4 <- function(...) {
  shiny::column(4, ...)
}

col_3 <- function(...) {
  shiny::column(3, ...)
}

col_2 <- function(...) {
  shiny::column(2, ...)
}

col_1 <- function(...) {
  shiny::column(1, ...)
}
