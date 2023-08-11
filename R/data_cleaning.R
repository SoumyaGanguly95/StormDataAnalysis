#' Clean Data
#'
#' This function removes rows with missing values and duplicate rows from a data frame.
#'
#' @param data A data frame containing the data to be cleaned.
#'
#' @return A data frame with missing values and duplicate rows removed.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' cleaned_data <- clean_data(loaded_data)
#' head(cleaned_data)
#' }
#'
#' @export
#' @importFrom dplyr distinct if_any everything
#' @usage clean_data(data)
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

  # Remove rows with missing values in numeric or logical columns
  cleaned_data <- data %>%
    dplyr::filter(if_any(everything(), not_missing))

  # Remove duplicate rows
  cleaned_data <- dplyr::distinct(cleaned_data)

  return(cleaned_data)
}
