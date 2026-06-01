#' Prepare data for heatmap plot
#'
#' Prepare data for rendering the heatmap plot view. It counts the total number
#' of events for each category in the lowest hierarchy level.
#'
#' @param data A dataframe obtained from \code{calculate_results()}.
#' @param heatmap_variable A character, the safety hierarchy variable. Can be
#' "AEDECOD", "AEDECOD_HLT" or "MLG_label".
#' @param heatmap_color Effect measure: "RR", "RD", "FDR" or "DFDR".
#' @param adae_data Original ADAE data, to obtain the PT-SOC equivalence.
#'
#' @returns A dataframe with unique combinations of MedDRA hierarchies and
#' a total count of events in each category
#'
prepare_heatmap_data <- function(
  data,
  heatmap_variable = c("AEDECOD", "AEDECOD_HLT", "MLG_label"),
  heatmap_color,
  adae_data
) {
  heatmap_variable <- match.arg(heatmap_variable)

  # Define which variables to keep distinct rows from the data
  if (heatmap_variable == "AEDECOD") {
    distinct_variables <- c("AEDECOD", "AEBODSYS")
  } else if (heatmap_variable == "AEDECOD_HLT") {
    distinct_variables <- c("AEDECOD", "MT_HLT", "MT_HLGT", "AEBODSYS")
  } else if (heatmap_variable == "MLG_label") {
    distinct_variables <- c("AEDECOD", "MLG_label", "SOC_MLG")
  }
  checkmate::assert_names(colnames(data), must.include = distinct_variables)
  # Remove SOCs not flagged by DFDR
  if (heatmap_color == "DFDR") {
    data <- data |>
      dplyr::filter(!is.na(.data$DFDR))
  }

  # Avoid errors when zero rows
  validate_need(nrow(data) > 0, "Data has zero rows")

  # Select variables and keep distinct rows
  data <- data |>
    # Remove missing values (e.g., outdated PTs from older MedDRA versions,
    # with no HLT or HLGT)
    dplyr::filter(dplyr::if_all(.cols = distinct_variables, .fns = \(x) {
      !is.na(x)
    })) |>
    dplyr::select(
      dplyr::all_of(distinct_variables),
      "trta_detector",
      "prob1",
      "prob2",
      "rr",
      "rd",
      "p",
      "p_adj",
      "p_adj_label",
      "big_n",
      "count",
      dplyr::any_of("pattime"), # Select patient-years if "incidence rates"
      # Select DFDR column if adjustment == "DFDR"
      dplyr::any_of(c("DFDR", "DFDR_label"))
    ) |>
    dplyr::distinct() # Keep unique combinations of safety variables hierarchy
  # Count the total count for each PT category
  totals <- data |>
    dplyr::count(.data$AEDECOD, wt = .data$count, name = "Total")
  # Join distinct rows with total counts
  results <- data |>
    dplyr::left_join(totals, by = "AEDECOD")
  if (heatmap_variable == "MLG_label") {
    # Categorize all PTs that are not inside MLGs inside a category called
    # "PTs not covered by MLG", inside each SOC_MLG
    results <- results |>
      dplyr::mutate(
        MLG_label = dplyr::case_when(
          stringr::str_detect(.data$MLG_label, "\\(PT\\)") ~
            stringr::str_c(
              "PTs not covered by MLG (",
              stringr::str_trunc(.data$SOC_MLG, 30),
              ")"
            ),
          stringr::str_detect(.data$MLG_label, "\\(MLG\\)") ~ .data$MLG_label
        )
      )
  }
  results
}

#' Count total adverse events by safety variable and treatment arm
#'
#' @param data A dataframe of adverse events, with columns: a safety variable
#' (AEBODSYS, MT_HLT, MT_HLGT, or SOC_MLG), "count", and
#' "trta_detector" (treatment arm)
#' @param variable A character, the safety variable to group the count
#'
#' @returns A wide dataframe with values: counts, rows: AE categories,
#' columns: treatment arms
count_events_by_arm <- function(data, variable) {
  data_wider <- data |>
    dplyr::count(
      .data[[variable]],
      .data$trta_detector,
      wt = .data$count,
      name = "count"
    ) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(variable),
      values_from = "count",
      names_from = "trta_detector"
    ) |>
    dplyr::mutate(
      verum_formatted = formatC(.data$Verum, big.mark = ","),
      comparator_formatted = formatC(.data$Comparator, big.mark = ",")
    )
  if (nrow(data_wider) > 0) {
    results <- data_wider |>
      dplyr::mutate(
        ratio = paste0(
          "(",
          .data$verum_formatted,
          "/",
          .data$comparator_formatted,
          ")"
        ),
        total = .data$Verum + .data$Comparator,
        hierarchy_level = variable # e.g., "AEDECOD" (character string)
      ) |>
      dplyr::rename(labels = dplyr::all_of(variable))
  } else {
    results <- NULL # when no DFDR significant p-values
  }
  results
}

#' Count total counts and ratios Comparator/Verum for heatmap map
#'
#' Creates the total counts of events in Comparator and Verum group in each of
#' the MedDRA hierarchy categories.
#'
#' @param data A dataframe after prepare_heatmap_data()
#' @param heatmap_variable A character, the safety hierarchy variable. Can be
#' "AEDECOD", "AEDECOD_HLT" or "MLG_label"
#'
#' @returns A dataframe of counts of events for each treatment arm
#' (and ratio) in each hierarchy category
count_heatmap_data <- function(
  data,
  heatmap_variable = c("AEDECOD", "AEDECOD_HLT", "MLG_label")
) {
  heatmap_variable <- match.arg(heatmap_variable)
  # Count total of events: one row per lowest-level AE category (PT),
  # one column per treatment arm
  count_pt <- count_events_by_arm(data, "AEDECOD")
  # Branching logic ----
  ## Case one: SOCs/PTs----
  if (heatmap_variable == "AEDECOD") {
    count_soc <- count_events_by_arm(data, "AEBODSYS")
    count_all <- dplyr::bind_rows(count_pt, count_soc)
  } else if (heatmap_variable == "MLG_label") {
    ## Case two: SOCs/MLGs/PTs----
    count_mlg <- count_events_by_arm(data, "MLG_label")
    count_soc_mlg <- count_events_by_arm(data, "SOC_MLG")
    count_all <- dplyr::bind_rows(count_pt, count_mlg, count_soc_mlg)
  } else if (heatmap_variable == "AEDECOD_HLT") {
    ## Case three: SOCs/HLGTs/HLTs/PTs----
    count_soc <- count_events_by_arm(data, "AEBODSYS")
    count_hlgt <- count_events_by_arm(data, "MT_HLGT")
    count_hlt <- count_events_by_arm(data, "MT_HLT")
    count_all <- dplyr::bind_rows(
      count_pt,
      count_hlt,
      count_hlgt,
      count_soc
    )
  }
  count_all
}


#' Get equivalence of children-parents hierarchical categories
#'
#' From a table, it extracts unique combinations of child-parent variables
#'
#' @param data A dataframe with a child variable and parent variable
#' @param child A character, the name of the child variable
#' @param parent A character, the name of the parent variable. If NULL,
#' the parent column will be an empty string
#'
#' @returns A data frame with two columns: "child" and "parent"
get_child_parent <- function(data, child, parent) {
  if (is.null(parent)) {
    # For the highest level, that doesn't have parent category
    data <- data |>
      dplyr::mutate(empty_parent = "")
    parent <- "empty_parent"
  }
  data |>
    dplyr::distinct(.data[[child]], .data[[parent]]) |>
    dplyr::rename(
      children = dplyr::all_of(child),
      parents = dplyr::all_of(parent)
    )
}

#' Get MedDRA equivalence children-parent category
#'
#' Obtains a dataframe of equivalence child-parent MedDRA category,
#' for a particular hierarchy
#'
#' @param data A dataframe with AE categories. Obtained after
#' prepare_heatmap_data()
#' @param heatmap_variable A character, the safety hierarchy variable. Can be
#' "AEDECOD", "AEDECOD_HLT" or "MLG_label"
get_parents_heatmap <- function(
  data,
  heatmap_variable = c("AEDECOD", "AEDECOD_HLT", "MLG_label")
) {
  heatmap_variable <- match.arg(heatmap_variable)
  # Hierarchy: PT - SOC
  if (heatmap_variable == "AEDECOD") {
    equivalence <- data |> dplyr::distinct(.data$AEDECOD, .data$AEBODSYS)
    parents_pt_soc <- equivalence |> get_child_parent("AEDECOD", "AEBODSYS")
    parents_soc_empty <- equivalence |> get_child_parent("AEBODSYS", NULL)
    parents <- dplyr::bind_rows(parents_pt_soc, parents_soc_empty)
  } else if (heatmap_variable == "AEDECOD_HLT") {
    # Hierarchy: PT - HLT - HLGT - SOC
    equivalence <- data |>
      dplyr::distinct(
        .data$AEDECOD,
        .data$MT_HLT,
        .data$MT_HLGT,
        .data$AEBODSYS
      )
    parents_pt_hlt <- equivalence |> get_child_parent("AEDECOD", "MT_HLT")
    parents_hlt_hlgt <- equivalence |> get_child_parent("MT_HLT", "MT_HLGT")
    parents_hlgt_soc <- equivalence |> get_child_parent("MT_HLGT", "AEBODSYS")
    parents_soc_empty <- equivalence |> get_child_parent("AEBODSYS", NULL)
    parents <- dplyr::bind_rows(
      parents_pt_hlt,
      parents_hlt_hlgt,
      parents_hlgt_soc,
      parents_soc_empty
    )
  } else if (heatmap_variable == "MLG_label") {
    # Hierarchy: PT - MLG - SOC_MLG
    equivalence <- data |>
      dplyr::distinct(.data$AEDECOD, .data$MLG_label, .data$SOC_MLG)
    parents_pt_mlg <- equivalence |> get_child_parent("AEDECOD", "MLG_label")
    parents_mlg_soc <- equivalence |> get_child_parent("MLG_label", "SOC_MLG")
    parents_soc_empty <- equivalence |> get_child_parent("SOC_MLG", NULL)
    parents <- dplyr::bind_rows(
      parents_pt_mlg,
      parents_mlg_soc,
      parents_soc_empty
    )
  }
  parents
}

#' Create treemap data
#' @param data A data prepared for heatmap, output from prepare_heatmap_data()
#' @param data_counts A count data, output from count_heatmap_data()
#' @param alpha Significance level.
#' @inheritParams prepare_heatmap_data
#'
make_treemap_data <- function(
  data,
  data_counts,
  heatmap_variable = c("AEDECOD", "AEDECOD_HLT", "MLG_label"),
  heatmap_color = c("RR", "RD", "FDR", "DFDR"),
  alpha
) {
  heatmap_variable <- match.arg(heatmap_variable)
  heatmap_color <- match.arg(heatmap_color)
  results <- data |>
    # Keep only useful variables: safety variable, RR/RD, adjusted p-values
    dplyr::select(
      "AEDECOD",
      "rr",
      "rd",
      "p_adj",
      "p_adj_label",
      dplyr::any_of(c("DFDR", "DFDR_label"))
    ) |>
    # Keep only one row per AE variable (instead of 2, one for arm)
    dplyr::distinct() |>
    dplyr::rename(labels = "AEDECOD")
  # Load the equivalence between children-parent MedDRA categories
  parents <- get_parents_heatmap(data = data, heatmap_variable)
  # Join all data
  data_all <- data_counts |>
    # Join with numerical results
    dplyr::left_join(results, by = "labels") |>
    # Join with children-parent equivalence
    dplyr::left_join(parents, by = c("labels" = "children")) |>
    #changes to variable names
    dplyr::mutate(
      # size of lowest level hierarchy boxes
      size = .data$total,
      size_formatted = formatC(.data$size, big.mark = ","),
      # size of upper hierarchies boxes
      size_upper = dplyr::case_when(
        .data$hierarchy_level == "AEDECOD" ~ .data$size,
        .data$hierarchy_level != "AEDECOD" ~ 0
      ),
      # Add a single highest hierarchy called "All"
      parents = dplyr::if_else(
        .data$hierarchy_level == "AEBODSYS",
        "All",
        .data$parents
      )
    )
  all <- data_all |>
    dplyr::filter(.data$hierarchy_level == "AEBODSYS") |>
    dplyr::summarise(
      Comparator = sum(.data$Comparator),
      Verum = sum(.data$Verum)
    ) |>
    dplyr::mutate(
      labels = "All",
      hierarchy_level = "All",
      verum_formatted = formatC(.data$Verum, big.mark = ","),
      comparator_formatted = formatC(.data$Comparator, big.mark = ","),
      ratio = paste0(
        "(",
        .data$verum_formatted,
        "/",
        .data$comparator_formatted,
        ")"
      ),
      total = .data$Comparator + .data$Verum,
      size = .data$total,
      size_formatted = formatC(.data$size, big.mark = ","),
      size_upper = 0
    ) |>
    dplyr::bind_rows(data_all)
  all
}
