#' Display Summary of DataFrame Columns
#'
#' This function displays a summary of the columns and their data types in the
#' provided dataframe.
#'
#' @param df The dataframe for which to display the summary.
#' @return None
#' @examples
#' \dontrun{
#' display_column_summary(cleaned_storm_data)
#' }
#' @export
display_column_summary <- function(df) {
  cat("Column Summary:\n")
  cat("----------------\n")
  for (col in names(df)) {
    cat("Column:", col, "\n")
    cat("Data Type:", class(df[[col]]), "\n")
    cat("----------------\n")
  }
}
