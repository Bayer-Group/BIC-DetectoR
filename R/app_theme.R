#' Application theme
#'
#' @noRd
detector_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#000000",
    primary = "#1E90FF", # blue logo
    secondary = "#985582", # purple logo
    danger = "#E72F32", # red logo
    info = "#1E90FF", # blue logo
    warning = "#E72F32", # red logo
    success = "#4C865F", # green complementary to purple logo
    base_font = bslib::font_collection(
      bslib::font_google("Noto Sans"),
      "Source Sans Pro",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    )
  )
}
