#' Draw volcano plot
#' @param data A dataframe.
#' @param effect_measure Either "RR" or "RD".
#' @param verum_name Custom verum name.
#' @param comparator_name Custom comparator name.
#' @param alpha Significance level.
draw_volcano <- function(
  data,
  effect_measure,
  verum_name,
  comparator_name,
  alpha
) {
  # Helpers for drawing auxiliary lines
  vline <- function(x = 0, color = "white") {
    list(
      type = "line",
      x0 = x,
      x1 = x,
      yref = "paper",
      y0 = 0,
      y1 = 1,
      line = list(color = color, dash = "dot")
    )
  }
  hline <- function(y = 0, color = "white", dash = "dot") {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(color = color, dash = dash)
    )
  }
  # Logic for differences between RR and RD
  effect_variable <- ifelse(effect_measure == "RR", "rr", "rd")
  xlab <- ifelse(
    effect_measure == "RR",
    "Relative Risk",
    "Risk difference (%)"
  )
  xaxis_type <- ifelse(effect_measure == "RR", "log", "linear")
  if (effect_measure == "RR") {
    max_rr <- data$rr |>
      log10() |>
      abs() |>
      max(na.rm = TRUE)
    range_x <- 10^max_rr * 1.2
    # At least show RR between 1/3 and 3
    max_x <- max(log10(range_x), log10(3))
    min_x <- min(-log10(range_x), -log10(3))
    x_vline_min <- 1 / 2
    x_vline_max <- 2
    tickvals <- c(0.2, 0.5, 1, 2, 5)
    effect_text <- "RR = %{x:.2f}"
  } else if (effect_measure == "RD") {
    max_rd <- data$rd |>
      abs() |>
      max(na.rm = TRUE)
    range_x <- max_rd * 1.2
    # At least show RD between -2% and 2%
    max_x <- max(range_x, 2)
    min_x <- min(-range_x, -2)
    x_vline_min <- 0
    x_vline_max <- 0
    tickvals <- c(-5, -2, -1, 0, 1, 2, 5)
    effect_text <- "RD = %{x:.2f}%"
  }

  tickformat <- ".1r" # one significant number
  hovertemplate <- paste0(
    data$axis_var,
    "<br>",
    "p = ",
    data$p_value_label,
    "<br>",
    "N (",
    verum_name,
    "/",
    comparator_name,
    ") = ",
    data$total,
    " (",
    data$verum,
    "/",
    data$comparator,
    ")",
    "<br>"
  )
  plot <- plotly::plot_ly(
    data = data,
    x = data[[effect_variable]],
    y = ~p,
    type = "scatter",
    mode = "markers",
    size = ~total,
    sizes = c(20, 1000),
    fill = ~"",
    marker = list(
      color = ~color,
      line = list(width = 0)
    ),
    hovertemplate = paste0(
      hovertemplate,
      effect_text,
      "<extra></extra>"
    ),
    height = 600
  ) |>
    plotly::layout(
      shapes = list(
        hline(y = 1, dash = "solid"),
        hline(y = alpha),
        vline(x = x_vline_min),
        vline(x = x_vline_max)
      ),
      xaxis = list(
        title = xlab,
        type = xaxis_type,
        range = list(min_x, max_x),
        fixedrange = TRUE,
        tickformat = tickformat,
        tickvals = tickvals
      ),
      yaxis = list(
        title = "p-value",
        tickvals = list(alpha),
        ticktext = list(paste0("p = ", alpha)),
        type = "log",
        autorange = "reversed",
        fixedrange = TRUE
      )
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      showAxisDragHandles = FALSE
    )
  plot
}
