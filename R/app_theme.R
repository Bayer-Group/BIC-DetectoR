#' Application theme
#'
#' @noRd
detector_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#000000",
    primary = "#D30F4B",
    secondary = "#5EA611",
    danger = "#B00020",
    info = "#08AFF6",
    warning = "#F09899",
    success = "#5EA611",
    base_font = bslib::font_collection(
      "Bayer Sans",
      bslib::font_google("Noto Sans"),
      "Source Sans Pro",
      "Helvetica Neue",
      "Arial",
      "sans-serif"
    )
  ) |>
    bslib::bs_add_variables(
      "dt-row-selected" = "131, 215, 250",
      "dt-row-selected-text" = "0, 0, 0",
      "plot-blue" = "#1E90FF",
      "plot-red" = "#E72F32",
      "plot-grid" = "#DDDDDD",
      "sidebar-bg" = "#007CBF",
      "sidebar-fg" = "#FFFFFF",
      "card-bg" = "#83D7FA",
      "card-border-color" = "#83D7FA",
      "popover-header-bg" = "#08AFF6",
      "popover-header-border-color" = "#D30F4B"
    ) |>
    bslib::bs_add_rules(
      paste(
        "[data-bs-theme=dark] {",
        "  --bs-body-bg: rgb(18, 18, 18);",
        "  --bs-body-color: rgba(255, 255, 255, 0.87);",
        "  --bs-primary: #f58ba3;",
        "  --bs-secondary: #c4e59b;",
        "  --bs-danger: #D57B8C;",
        "  --bs-info: #62bbff;",
        "  --bs-warning: #F09899;",
        "  --bs-success: #c4e59b;",
        "  --bs-tertiary-color: rgba(255, 255, 255, 0.60);",
        "  --bs-border-color: #242424;",
        "  --bs-emphasis-color: rgb(255, 255, 255);",
        "  --bs-sidebar-bg: #1e1e1e;",
        "  --bs-sidebar-fg: rgba(255, 255, 255, 0.87);",
        "  --bs-card-bg: #1e1e1e;",
        "  --bs-card-border-color: #1e1e1e;",
        "  --bs-popover-header-bg: #2c2c2c;",
        "  --bs-popover-header-border-color: #f58ba3;",
        "  --plot-blue: #62bbff;",
        "  --plot-red: #F09899;",
        "  --plot-grid: #333333;",
        "}"
      )
    )
}
