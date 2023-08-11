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
#' @importFrom dplyr filter_all all_vars distinct
#' @usage clean_data(data)
clean_data <- function(data) {
  # Remove rows with missing values
  cleaned_data <- dplyr::filter_all(data, all_vars(function(x) !is.na(x)))


  # Remove duplicate rows
  cleaned_data <- dplyr::distinct(cleaned_data)

  return(cleaned_data)
}
