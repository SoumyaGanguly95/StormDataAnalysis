#' Clean Data Function
#'
#' This function cleans the input data by checking specified columns for valid entries,
#' filtering rows with valid entries if they are more than 50% of the total entries,
#' and removing duplicate rows.
#'
#' @param data A data frame containing the input data.
#' @return A cleaned data frame.
#'
#' @import dplyr
#'
#' @examples
#' data_to_clean <- data.frame(
#'   tor_f_scale = c(1, 2, NA, 4),
#'   tor_length = c(3.4, 5.2, 2.8, NA),
#'   tor_width = c(0.8, NA, 1.1, 2.0),
#'   tor_other_wfo = c("A", "B", "C", "A")
#' )
#' cleaned_data <- clean_data(data_to_clean)
#'
#' @export
clean_data <- function(data) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required but not installed.")
  }

  # Define a custom function to check for non-missing and non-NA values in numeric or logical columns
  not_missing <- function(x) {
    if (is.numeric(x) || is.logical(x)) {
      !is.na(x)
    } else {
      TRUE
    }
  }

  # Check the specified columns for valid entries
  valid_entries <- data %>%
    dplyr::filter(
      not_missing(data$tor_f_scale) &
        not_missing(data$tor_length) &
        not_missing(data$tor_width) &
        not_missing(data$tor_other_wfo)
    )

  total_rows <- nrow(data)
  valid_rows <- nrow(valid_entries)

  # Check if the number of valid entries is greater than 50% of total entries
  if (valid_rows > 0.5 * total_rows) {
    # Filter out rows with valid entries
    cleaned_data <- data %>%
      dplyr::semi_join(valid_entries)
  } else {
    # Drop the specified columns
    cleaned_data <- data %>%
      dplyr::select(-c(tor_f_scale, tor_length, tor_width, tor_other_wfo))
  }

  # Remove duplicate rows
  cleaned_data <- dplyr::distinct(cleaned_data)

  return(cleaned_data)
}
