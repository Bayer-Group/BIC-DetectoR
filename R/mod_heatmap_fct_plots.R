#' Add hover text to heatmap data
#' @param data Dataframe from make_treemap_data()
#' @param verum_name Custom name for Verum arm.
#' @param comparator_name Custom name for Comparator arm.
#' @inheritParams prepare_heatmap_data
add_heatmap_hover_text <- function(
  data,
  heatmap_color,
  verum_name = "Verum",
  comparator_name = "Comparator"
) {
  # Validate if empty
  empty_msg <- paste0(
    "No heatmap to display: DFDR-adjusted p-values are all missing ",
    "or no events remain after filtering."
  )
  validate_need(
    NROW(data) > 0,
    empty_msg
  )
  checkmate::assert_names(colnames(data), must.include = c("size", "ratio"))
  assertthat::assert_that(any(c("rr", "rd") %in% colnames(data)))
  assertthat::assert_that(any(
    c("p_adj_label", "DFDR_label") %in% colnames(data)
  ))
  # Extract number of repeated parent values (should be zero)
  repeated_labels <- data |>
    dplyr::count(.data$labels) |>
    dplyr::filter(.data$n > 1) |>
    dplyr::pull("labels")
  repeated_data <- data |>
    dplyr::filter(.data$labels %in% repeated_labels)
  repeated_children <- repeated_data$labels
  repeated_parents <- repeated_data$parents
  validate_need(
    object = length(repeated_labels) == 0,
    label = paste0(
      "WARNING: Not all 'labels' have unique parent values!
              Following values have multiple parent values: label - '",
      repeated_children,
      "' has parent value - '",
      repeated_parents,
      "'"
    )
  )

  # Add text visible on the rectangles and hovertext
  data_hover <- data |>
    dplyr::mutate(
      text_full = paste0(
        "Total: ",
        .data$size_formatted,
        "<br>(",
        verum_name,
        "/",
        comparator_name,
        "):<br>",
        .data$ratio,
        "<br>"
      )
    )
  # Refine hovertext and colorscale
  if (heatmap_color == "RR") {
    data_hover <- data_hover |>
      dplyr::mutate(
        rr2 = format_decimals(.data$rr, 2),
        text_result = dplyr::case_when(
          .data$hierarchy_level != "AEDECOD" ~ "",
          .default = paste0("RR = ", .data$rr2)
        ),
        result_num = dplyr::case_when(
          .data$rr > 1 ~ .data$rr,
          .data$rr <= 1 ~ 1, # Color all RR <= 1 in the same shade
          is.na(.data$rr) ~ 0.9 # Background color for higher hierarchies
        ),
        scale_num = log(.data$result_num)
      )
  } else if (heatmap_color == "RD") {
    data_hover <- data_hover |>
      dplyr::mutate(
        rd2 = format_decimals(.data$rd, 2),
        text_result = dplyr::case_when(
          .data$hierarchy_level != "AEDECOD" ~ "",
          .default = paste0("RD = ", .data$rd2)
        ),
        result_num = dplyr::case_when(
          .data$rd > 0 ~ .data$rd,
          .data$rd <= 0 ~ 0, # Color all RD <= 0 in the same shade
          is.na(.data$rd) ~ -0.1 # Background color for higher hierarchies
        ),
        scale_num = .data$result_num
      )
  } else if (heatmap_color == "FDR") {
    data_hover <- data_hover |>
      dplyr::mutate(
        text_result = dplyr::case_when(
          .data$hierarchy_level != "AEDECOD" ~ "",
          .default = paste0("Adj. p = ", .data$p_adj_label)
        ),
        result_num = dplyr::case_when(
          is.na(.data$p_adj) ~ 1.1, # Background color for higher hierarchies
          .default = .data$p_adj
        ),
        scale_num = log(.data$result_num)
      )
  } else if (heatmap_color == "DFDR") {
    validate_need(
      "DFDR" %in% colnames(data_hover),
      empty_msg
    )
    data_hover <- data_hover |>
      dplyr::mutate(
        text_result = dplyr::case_when(
          .data$hierarchy_level != "AEDECOD" | is.na(.data$DFDR) ~ "",
          .default = paste0("Adj. p = ", .data$DFDR_label)
        ),
        result_num = dplyr::case_when(
          is.na(.data$DFDR) ~ 1.1, # Background color for higher hierarchies
          .default = .data$DFDR
        ),
        scale_num = log(.data$result_num)
      )
  }
  data_hover
}

#' Color palette for heatmap plot
#'
#' @param data A dataframe with heatmap data.
#' @param heatmap_mode A character, can be "RR", "RD", "FDR", "DFDR".
#' @param alpha Significance level.
#' @param theme Either "light" or "dark".
add_heatmap_colors <- function(
  data,
  heatmap_mode = c("RR", "RD", "FDR", "DFDR"),
  alpha,
  theme = c("light", "dark")
) {
  theme <- match.arg(theme)
  # Add colors for scale
  colpal <- create_heatmap_palette(theme = theme)
  color_background <- ifelse(theme == "light", "#fff", "#121212")
  if (heatmap_mode %in% c("RR", "RD")) {
    # Cut the effect measure in 12 bins
    if (heatmap_mode == "RR") {
      max_log_rr <- log(max(data$rr, na.rm = TRUE))
      log_seq <- seq(log(1), max_log_rr, length = 13)
      ticks <- exp(log_seq)
    } else if (heatmap_mode == "RD") {
      max_rd <- max(data$rd, na.rm = TRUE)
      ticks <- seq(0, max_rd, length = 13)
    }

    target_name <- tolower(heatmap_mode) # "rr" or "rd"
    target_variable <- data[[target_name]]

    # Assigns colors to effect measure
    data <- data |>
      dplyr::mutate(
        color = dplyr::case_when(
          target_variable <= ticks[1] ~ colpal[1],
          target_variable <= ticks[2] ~ colpal[12],
          target_variable <= ticks[3] ~ colpal[13],
          target_variable <= ticks[4] ~ colpal[14],
          target_variable <= ticks[5] ~ colpal[15],
          target_variable <= ticks[6] ~ colpal[16],
          target_variable <= ticks[7] ~ colpal[17],
          target_variable <= ticks[8] ~ colpal[18],
          target_variable <= ticks[9] ~ colpal[19],
          target_variable <= ticks[10] ~ colpal[20],
          target_variable <= ticks[11] ~ colpal[21],
          target_variable <= ticks[12] ~ colpal[22],
          target_variable > ticks[12] ~ colpal[23],
          is.na(target_variable) ~ color_background,
          .data$hierarchy_level != "AEDECOD" ~ color_background
        )
      )
  } else if (heatmap_mode %in% c("FDR", "DFDR")) {
    if (heatmap_mode == "FDR") {
      target_variable <- data[["p_adj"]]
    } else if (heatmap_mode == "DFDR") {
      target_variable <- data[["DFDR"]]
    }

    if (alpha == "0.05") {
      data <- data |>
        dplyr::mutate(
          color = dplyr::case_when(
            target_variable > 0.05 ~ colpal[1],
            target_variable > 0.01 ~ colpal[11],
            target_variable > 0.001 ~ colpal[14],
            target_variable > 0.0001 ~ colpal[17],
            target_variable > 0.00001 ~ colpal[21],
            target_variable <= 0.00001 ~ colpal[23],
            is.na(target_variable) ~ color_background,
            .data$hierarchy_level != "AEDECOD" ~ color_background
          )
        )
    } else if (alpha == "0.01") {
      data <- data |>
        dplyr::mutate(
          color = dplyr::case_when(
            target_variable > 0.01 ~ colpal[1],
            target_variable > 0.001 ~ colpal[11],
            target_variable > 0.0001 ~ colpal[15],
            target_variable > 0.00001 ~ colpal[19],
            target_variable <= 0.00001 ~ colpal[23],
            is.na(target_variable) ~ color_background,
            .data$hierarchy_level != "AEDECOD" ~ color_background
          )
        )
    } else if (alpha == "0.1") {
      data <- data |>
        dplyr::mutate(
          color = dplyr::case_when(
            target_variable > 0.1 ~ colpal[1],
            target_variable > 0.01 ~ colpal[11],
            target_variable > 0.001 ~ colpal[14],
            target_variable > 0.0001 ~ colpal[17],
            target_variable > 0.00001 ~ colpal[21],
            target_variable <= 0.00001 ~ colpal[23],
            is.na(target_variable) ~ color_background,
            .data$hierarchy_level != "AEDECOD" ~ color_background
          )
        )
    }
  }
  data
}


#' Create a discrete color palette for heatmap
#' @param theme Either "light" or "dark"
create_heatmap_palette <- function(theme = c("light", "dark")) {
  theme <- match.arg(theme)
  if (theme == "light") {
    color_blue <- "#1E90FF"
    color_red <- "#E72F32"
  } else {
    color_blue <- "#62bbff"
    color_red <- "#F09899"
  }
  n_color_steps <- 23
  colpal <- grDevices::colorRampPalette(c(color_blue, color_red))(n_color_steps)
  colpal
}


#' Create legend for heatmap
#' @param data Dataframe for treemap.
#' @param heatmap_color A character: "RD", "RR", "FDR" or "DFDR".
#' @param alpha Numeric. Significance level.
#' @param verum_name Custom Verum name.
#' @param comparator_name Custom Comparator name.
#' @inheritParams draw_treemap
make_treemap_legend <- function(
  data,
  heatmap_color,
  alpha,
  theme,
  verum_name = "Verum",
  comparator_name = "Comparator"
) {
  # Legend title
  if (heatmap_color == "RR") {
    legend_title <- "Relative Risk"
  } else if (heatmap_color == "RD") {
    legend_title <- "Risk Difference (%)"
  } else if (heatmap_color == "FDR") {
    legend_title <- "p-value (FDR-adjusted)"
  } else if (heatmap_color == "DFDR") {
    legend_title <- "p-value (DFDR-adjusted)"
  }
  # Legend ticks
  if (heatmap_color %in% c("FDR", "DFDR")) {
    # p-value range from 0 to 1
    legend_max <- 1
  } else if (heatmap_color %in% c("RD", "RR")) {
    # get the range from the effect value (either rr or rd)
    effect_variable <- tolower(heatmap_color)
    legend_max <- max(data[[effect_variable]], na.rm = TRUE)
  }
  if (heatmap_color == "RR") {
    # Write labels in the small colored boxes
    tick_labels <- c(
      "<=1",
      round(exp(seq(log(1), log(legend_max), length = 13)), 1)[-1]
    )
    # Remove 1 from scale, to avoid overlapping labels
    tick_labels <- tick_labels[tick_labels != "1"]
    tick_values <- log(readr::parse_number(tick_labels))
  } else if (heatmap_color == "RD") {
    ticks <- seq(0, legend_max, length = 13)
    ticks <- format_decimals(ticks, 2)
    tick_labels <- c("<=0", ticks[-1])
    # Remove 0 from scale, to avoid overlapping labels
    tick_labels <- tick_labels[tick_labels != "0"]
    tick_values <- readr::parse_number(tick_labels)
  } else if (heatmap_color %in% c("FDR", "DFDR")) {
    if (alpha == 0.05) {
      tick_labels <- c(
        "1",
        "0.05",
        "0.001",
        "0.0001",
        "0.00001"
      )
    } else if (alpha == 0.01) {
      tick_labels <- c(
        "1",
        "0.01",
        "0.001",
        "0.0001",
        "0.00001"
      )
    } else if (alpha == 0.1) {
      tick_labels <- c(
        "1",
        "0.1",
        "0.01",
        "0.001",
        "0.0001",
        "0.00001"
      )
    }
    tick_values <- log(readr::parse_number(tick_labels))
  }
  color_palette <- create_heatmap_palette(theme = theme)
  # Prepare for plotly
  # Plotly heatmap need data in matrix format
  vals <- matrix(tick_values, nrow = 1)
  labels <- matrix(tick_labels, nrow = 1)

  # Keep stronger blue and redder shades for contrast
  colors <- color_palette[c(1, 12:23)]

  # Revert colors for p-values (lower values = more signal)
  if (heatmap_color %in% c("FDR", "DFDR")) {
    colors <- rev(colors)
  }

  caption_left <- ifelse(
    heatmap_color %in% c("RR", "RD"),
    paste0("Favors ", verum_name),
    "Non-significant"
  )
  caption_right <- ifelse(
    heatmap_color %in% c("RR", "RD"),
    paste0("Favors ", comparator_name),
    "Statistically Significant"
  )

  # Plotly ----
  plotly::plot_ly(
    z = vals,
    type = "heatmap",
    showscale = FALSE,
    text = labels,
    colors = colors,
    texttemplate = "%{text}",
    hoverinfo = "none",
    height = 150
  ) |>
    plotly::layout(
      xaxis = list(
        title = "",
        showticklabels = FALSE,
        ticks = "",
        fixedrange = TRUE
      ),
      yaxis = list(
        title = "",
        showticklabels = FALSE,
        ticks = "",
        fixedrange = TRUE
      ),
      title = legend_title,
      annotations = list(
        list(
          x = 0,
          y = -0.02,
          xref = "paper",
          yref = "paper",
          text = caption_left,
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 12)
        ),
        list(
          x = 1,
          y = -0.02,
          xref = "paper",
          yref = "paper",
          text = caption_right,
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "top",
          font = list(size = 12)
        )
      ),
      margin = list(b = 60, t = 60, r = 20, l = 20)
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      showAxisDragHandles = FALSE
    )
}


#' Draw a treemap plot
#' @param data Data prepared for treemap.
#' @param heatmap_color "RD", "RR", "FDR" or "DFDR".
#' @param theme Either "light" or "dark".
draw_treemap <- function(
  data,
  heatmap_color,
  theme = c("light", "dark")
) {
  theme <- match.arg(theme)
  color_background <- ifelse(theme == "light", "#fff", "#121212")
  # Create the plot
  fig <- plotly::plot_ly(
    data,
    type = "treemap",
    labels = ~labels,
    parents = ~parents,
    values = ~size_upper,
    marker = list(
      colors = ~color
    ),
    customdata = ~text_full,
    hovertemplate = "<b>%{label}</b><br>%{customdata}<br>%{text}<extra></extra>",
    text = ~text_result,
    pathbar = list(visible = FALSE),
    root = list(color = color_background)
  ) |>
    plotly::layout(
      margin = list(l = 20, r = 20, t = 0, b = 0)
    ) |>
    plotly::config(displayModeBar = FALSE)
  fig
}
