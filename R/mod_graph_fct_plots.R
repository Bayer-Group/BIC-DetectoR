#' Theme for double-dot plots (double_dot_plot and effect_plot) for plotly
#'@param p A plotly object.
#'
#'@noRd
layout_detector <- function(p) {
  layout <- plotly::add_trace(
    # Enables second duplicated axis
    p,
    xaxis = "x2",
    opacity = 0
  ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(
        fixedrange = TRUE
      ),
      xaxis2 = list(
        fixedrange = TRUE,
        showgrid = FALSE,
        overlaying = "x",
        side = "top",
        automargin = TRUE
      ),
      yaxis = list(
        title = "",
        fixedrange = TRUE
      ),
      font = list(
        size = 12
      ),
      margin = list(l = 5, r = 5, t = 0, b = 0)
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      showAxisDragHandles = FALSE
    )
  layout
}

#' A plotly function to plot the double_dot_plot
#'
#'@param data Arranged data set from arrange_data().
#'@param label_collapsed Logical, using collapsed labels or full label.
#'@param frequency_measure Measure variable (proportions or incidence rates).
#'@param height Height of the plot in pixels.
#'@return A plotly object
#'
make_double_dot_plotly <- function(
  data,
  label_collapsed,
  frequency_measure = c("proportions", "incidence rates"),
  height
) {
  frequency_measure <- match.arg(frequency_measure)
  xaxis_label <- ifelse(
    frequency_measure == "proportions",
    "Proportion",
    "Incidence rate (per 100 patient-years)"
  )
  data <- data |>
    dplyr::mutate(
      truncated_labels = stringr::str_trunc(
        as.character(.data$axis_var),
        20
      ),
      # color labels conditionally on p-value to highlight signals
      # instead of directly adding color, flags the element with HTML attributes
      # so it can be detected and colored by the CSS file later
      label_prefix = dplyr::case_when(
        .data$col_p == "red" ~ "<span detector='favours-comparator'>",
        .data$col_p == "blue" ~ "<span detector='favours-verum'>",
        .data$col_p == "white" ~ "<span>"
      ),
      full_labels = paste0(
        .data$label_prefix,
        .data$asterisks,
        .data$axis_var,
        "</span>"
      ),
      truncated_labels = paste0(
        .data$label_prefix,
        .data$asterisks,
        .data$truncated_labels,
        "</span>"
      )
    )
  if (frequency_measure == "proportions") {
    data <- data |>
      dplyr::mutate(
        hover_text = stringr::str_glue(
          "{.data$axis_var}<br>{.data$trta_detector} ({.data$count}/{.data$big_n})"
        )
      )
  } else if (frequency_measure == "incidence rates") {
    data <- data |>
      dplyr::mutate(
        hover_text = stringr::str_glue(
          "{.data$axis_var}<br>{.data$trta_detector} ({.data$count}/{.data$pattime})"
        )
      )
  }
  # white, red and blue are flag colors, the final colors are controlled in the
  # CSS file
  pal <- c("red", "blue")
  pal <- stats::setNames(pal, c("Comparator", "Verum"))
  max_value <- max(data$prop, na.rm = TRUE)
  padding <- max_value * 0.05
  x_min <- 0 - padding # Force start at 0%
  x_max <- max_value + padding
  # Control the number of decimal places to avoid repeated labels "0%" with
  # very small numbers
  x_tick_format <- dplyr::case_when(
    frequency_measure == "proportions" & x_max > 0.05 ~ ".0%",
    frequency_measure == "proportions" & x_max > 0.005 ~ ".1%",
    frequency_measure == "proportions" & x_max <= 0.005 ~ ".2%",
    frequency_measure == "incidence rates" & x_max > 5 ~ ".0f",
    frequency_measure == "incidence rates" & x_max > 0.5 ~ ".1f",
    frequency_measure == "incidence rates" & x_max <= 0.5 ~ ".2f"
  )
  fig <- data |>
    plotly::plot_ly(
      x = ~prop,
      xaxis = "x",
      y = ~axis_var,
      customdata = ~hover_text, # keep full labels for hover text
      name = ~trta_detector,
      color = ~trta_detector,
      colors = pal,
      symbol = ~trta_detector,
      symbols = c(17, 16),
      hovertemplate = paste(
        "%{customdata}:",
        if (frequency_measure == "proportions") "%{x:.1%}" else "%{x:.1f}",
        "<extra></extra>"
      ),
      type = "scatter",
      mode = "markers",
      size = 10,
      height = height
    ) |>
    plotly::layout(
      xaxis = list(
        title = xaxis_label,
        tickformat = x_tick_format,
        range = list(x_min, x_max),
        tick0 = 0
      ),
      xaxis2 = list(
        title = xaxis_label,
        tickformat = x_tick_format,
        range = list(x_min, x_max),
        tick0 = 0
      ),
      yaxis = list(
        tickmode = "array",
        tickvals = ~axis_var,
        ticktext = if (label_collapsed) ~truncated_labels else ~full_labels,
        autorange = "reversed" # to preserve order
      )
    ) |>
    layout_detector()

  fig
}
#' Helper function to add hover text to the effect plot
#' @param data A dataframe.
#' @param display Either "RD" or "RR".
#' @param adjustment "FDR" or "DFDR"
add_hover_text <- function(
  data,
  display = c("RD", "RR"),
  adjustment = c("FDR", "DFDR")
) {
  display <- match.arg(display)
  adjustment <- match.arg(adjustment)
  if (display == "RD") {
    estimate <- "rd"
    lower <- "rd_lcl"
    upper <- "rd_ucl"
    symbol <- "%"
    digits <- 1
  } else if (display == "RR") {
    estimate <- "rr"
    lower <- "rr_lcl"
    upper <- "rr_ucl"
    symbol <- ""
    digits <- 2
  }
  adjustment_variable <- ifelse(
    adjustment == "FDR",
    "p_adj_label",
    "DFDR_label"
  )
  data |>
    dplyr::mutate(
      hover_text = paste0(
        toupper(display),
        ": ",
        format_decimals(.data[[estimate]], digits),
        symbol,
        ", ",
        "CI: (",
        format_decimals(.data[[lower]], digits),
        symbol,
        ", ",
        format_decimals(.data[[upper]], digits),
        symbol,
        ")",
        "<br>",
        "p-value (unadjusted): ",
        .data$p_value_label,
        .data$asterisks,
        "<br>",
        "p-value (adjusted): ",
        .data[[adjustment_variable]],
        .data$asterisks
      )
    )
}
#' A plotly function to plot the effect plot (RD or RR)
#'
#'@param data Arranged data set from arrange_data().
#'@param display RD (Risk Difference) or RR (Relative Risk).
#'
#'@return A plotly object.
#'
#'@noRd
make_effect_plotly <- function(
  data,
  display = c("RD", "RR"),
  adjustment = c("FDR", "DFDR"),
  height
) {
  display <- match.arg(display)
  adjustment <- match.arg(adjustment)
  xintercept <- ifelse(display == "RD", 0, 1)
  xaxis_label <- ifelse(display == "RD", "Risk difference (%)", "Relative Risk")
  # Set ranges according to most extreme confidence interval
  range_x <- ifelse(
    display == "RR",
    10^(max(abs(log10(c(data$rr_ucl, data$rr_lcl))), na.rm = TRUE)) * 1.2,
    max(abs(c(data$rd_ucl, data$rd_lcl)), na.rm = TRUE) * 1.2
  )
  max_x <- ifelse(display == "RR", log10(range_x), range_x)
  min_x <- ifelse(display == "RR", -log10(range_x), -range_x)
  # Conditionally color significant results
  # white, red and blue are flag colors, the final colors are controlled in the
  # CSS file
  pal <- c("white", "red", "blue")
  pal <- stats::setNames(pal, c("white", "red", "blue"))
  # Keep only one row per category
  data <- data |>
    dplyr::distinct(.data$axis_var, .keep_all = TRUE)
  if (display == "RD") {
    data_hover <- data |>
      add_hover_text(display = display, adjustment = adjustment)
    plot <- data_hover |>
      plotly::plot_ly(
        x = ~rd,
        xaxis = "x",
        y = ~axis_var,
        type = "scatter",
        mode = "markers",
        color = ~col_p,
        colors = pal,
        height = height,
        customdata = ~hover_text,
        hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
      ) |>
      plotly::add_segments(
        x = ~rd_lcl,
        xend = ~rd_ucl,
        y = ~axis_var,
        yend = ~axis_var
      )
  } else if (display == "RR") {
    data_hover <- data |>
      add_hover_text(display = display, adjustment = adjustment)
    plot <- data_hover |>
      plotly::plot_ly(
        x = ~rr,
        xaxis = "x",
        y = ~axis_var,
        type = "scatter",
        mode = "markers",
        color = ~col_p,
        colors = pal,
        height = height,
        customdata = ~hover_text,
        hovertemplate = "%{y}<br>%{customdata}<extra></extra>"
      ) |>
      plotly::add_segments(
        x = ~rr_lcl,
        xend = ~rr_ucl,
        y = ~axis_var,
        yend = ~axis_var
      ) |>
      plotly::layout(
        xaxis = list(
          tickvals = c(0.1, 0.5, 1, 2, 10),
          type = "log"
        ),
        xaxis2 = list(
          tickvals = c(0.1, 0.5, 1, 2, 10),
          type = "log"
        )
      )
  }
  plot <- plot |>
    layout_detector() |>
    plotly::layout(
      xaxis = list(
        title = xaxis_label,
        range = c(min_x, max_x)
      ),
      xaxis2 = list(
        title = xaxis_label,
        range = c(min_x, max_x)
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE
      ),
      shapes = list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = xintercept,
        x1 = xintercept,
        line = list(
          dash = "dot",
          width = 1
        )
      )
    )
  plot
}
# Theme and color picker for plots ----

# #' Create an empty plot
# #'
# #'@param label A character string with the text to show in the plot.
# #'
# #'@return An empty plot
# #'
# #'@noRd
# empty_plotly <- function(label) {
#   p <- plotly::plotly_empty(type = "scatter", mode = "markers") |>
#     plotly::config(
#       displayModeBar = FALSE
#     ) |>
#     plotly::layout(
#       title = list(
#         text = label,
#         yref = "paper",
#         y = 0.5
#       )
#     )
#   p
# }

#' Flags p-values by statistical significance
#'
#' @param data A dataframe of results. Must include columns called "p" and
#' "p_adj"
#' @param adjustment A character, "FDR" or "DFDR"
#' @param effect_measure Either "RR" or "RD"
#' @param alpha Numeric, the significance level
#'
#' @returns A dataframe with colors for p-values in "col_p", and for adjusted
#' p-values in "col_p_adj"
#'
flag_significant <- function(
  data,
  adjustment = c("FDR", "DFDR"),
  effect_measure = c("RD", "RR"),
  alpha
) {
  adjustment <- match.arg(adjustment)
  effect_measure <- match.arg(effect_measure)
  p_adj_variable <- ifelse(adjustment == "FDR", "p_adj", "DFDR")
  effect_variable <- ifelse(effect_measure == "RD", "rd", "rr")
  effect_null <- ifelse(effect_measure == "RD", 0, 1)
  data <- data |>
    dplyr::mutate(
      effect_direction = dplyr::case_when(
        .data[[effect_variable]] < effect_null ~ "Favours verum",
        .data[[effect_variable]] >= effect_null ~ "Favours comparator"
      ),
      asterisks = dplyr::case_when(
        .data[[p_adj_variable]] <= alpha ~ "**",
        .data$p <= alpha ~ "*",
        .default = ""
      ),
      col_p = dplyr::case_when(
        .data$p <= alpha & .data$effect_direction == "Favours verum" ~ "blue",
        .data$p <= alpha &
          .data$effect_direction == "Favours comparator" ~
          "red",
        .default = "white"
      ),
      col_p_adj = dplyr::case_when(
        .data[[p_adj_variable]] <= alpha &
          .data$effect_direction == "Favours verum" ~
          "blue",
        .data[[p_adj_variable]] <= alpha &
          .data$effect_direction == "Favours comparator" ~
          "red",
        .default = "white"
      )
    )
  data
}
