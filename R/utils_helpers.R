#' Calculate the minimum number of events in verum
#'
#' @param n1 Number of patients in Verum
#' @param n2 Number of patients in Comparator
#' @param alternative A character string describing the alternative hypothesis
#' (one or two sided)
#' @param alpha Alpha level of the test
#' @return minimum number of events in verum
#'
find_min_event <- function(
  n1,
  n2,
  alternative = c("two.sided", "less"),
  alpha = 0.05
) {
  alternative <- match.arg(alternative)
  # Create a numeric vectors of events, to try which yields significant results
  if (n1 < 1000) {
    x1 <- seq(1, round(0.1 * n1, 0), 1)
  } else if (n1 < 10000) {
    x1 <- seq(1, round(0.05 * n1, 0), 1)
  } else if (n1 < 100000) {
    x1 <- seq(1, round(0.01 * n1, 0), 1)
  } else {
    x1 <- seq(1, round(0.001 * n1, 0), 1)
  }
  # Get p-values from Fisher's test for each number of events
  p_values <- sapply(
    x1,
    FUN = function(x1) {
      stats::fisher.test(
        matrix(c(0, n2, x1, n1 - x1), nrow = 2),
        alternative = alternative
      )$p.value
    }
  )
  # Get minimum number that yields significant result; return NA if none significant
  sig <- which(p_values < alpha)
  if (length(sig) == 0) {
    return(NA_integer_)
  }
  min(sig)
}

# #' Purpose: get exponent of the mean of a variable in scientific notation
# #'          for adaptive rounding
# #'          When abs(mean)>=1, 0 is returned
# #'@param x numeric value
# #'@return integer
# get_exponent <- function(x) {
#   if (x == 0) {
#     return(0) # Define the exponent for 0 as 0
#   }
#   # Convert the number to scientific notation
#   sci_notation <- format(x, scientific = TRUE)
#   # Use a regular expression to extract the exponent
#   exponent <- as.numeric(sub(".*e([+-]?[0-9]+)", "\\1", sci_notation))
#   min(0, exponent)
# }

#' Format p-values to four decimal places
#'
#' @param p A numeric vector of p-values
#' @returns A character vector of p-values rounded to four decimal places, or
#' "<0.0001"
#'
format_p_values <- function(p) {
  dplyr::case_when(
    is.na(p) ~ "",
    round(p, 4) == 0 ~ "<0.0001",
    TRUE ~ format(round(p, 4), nsmall = 4, scientific = FALSE)
  )
}

#' Format number to decimal places
#' @param number A numeric vector
#' @param digits Number of decimal places
format_decimals <- function(number, digits) {
  dplyr::case_when(
    is.na(number) ~ "",
    TRUE ~ format(round(number, digits), nsmall = digits, scientific = FALSE)
  )
}

#' tryCatch an expression, returns NULL if error
#' @param exp An expression.
try_null <- function(exp) {
  tryCatch(
    {
      exp
    },
    error = function(cnd) NULL
  )
}

#' Catch an error message
#' @param exp An expression.
catch_error_message <- function(exp) {
  rlang::catch_cnd(exp)$message
}

#' Show error message
#' @param error_message A character, the error message.
#' @param label A text to show before the error message.
#'
show_error <- function(error_message, label) {
  shiny::validate(
    shiny::need(
      is.null(error_message),
      message = paste0(label, error_message)
    )
  )
}

#' A shortcut for validate(need())
#' @param object The object needed.
#' @param label A text to show when the object is not validated.
validate_need <- function(object, label) {
  shiny::validate(
    shiny::need(object, message = label)
  )
}

#' Custom assert that required columns are present in data
#' @param data A dataframe.
#' @param req_columns A character vector of column names required.
assert_columns <- function(data, req_columns) {
  data_columns <- colnames(data)
  if (!all(req_columns %in% data_columns)) {
    missing_columns <- req_columns[!req_columns %in% data_columns]
    rlang::abort(
      message = paste0(
        "Missing variables: ",
        paste(missing_columns, collapse = ", ")
      ),
      class = "missing-columns"
    )
  }
}

#' Assert that an object is not null
#' @param object The object.
#'
assert_is_not_null <- function(object) {
  object_name <- deparse(substitute(object))
  assertthat::assert_that(
    !is.null(object),
    msg = paste0("`", object_name, "` must not be NULL!")
  )
}


#' Extract variable names and labels to show as options in pickerInput
#'
#' @param data A dataframe (ADAE or ADSL)
#'
get_variable_labels <- function(data) {
  data_variables_tmp <- purrr::map(
    data,
    function(x) attr(x, "label", exact = TRUE)
  )
  data_variables <- names(data_variables_tmp)
  names(data_variables) <- paste0(
    names(data_variables_tmp),
    ifelse(
      as.character(data_variables_tmp) == "NULL",
      "",
      paste0(" - ", as.character(data_variables_tmp))
    )
  )
  data_variables
}

#' Reorder a vector to show sensible defaults first, for text inputs (e.g.,
#' variable names)
#' @param vector A character vector to reorder.
#' @param pattern A character, regex pattern to select sensible defaults.
#' @returns A reordered vector with elements containing the pattern in first
#' position.
reorder_pattern <- function(vector, pattern) {
  vector_with_pattern <- vector[stringr::str_detect(vector, pattern)]
  vector_without_pattern <- vector[!stringr::str_detect(vector, pattern)]
  vector_reordered <- c(vector_with_pattern, vector_without_pattern)
  vector_reordered
}
