#' Merge Cleaned Datasets
#'
#' This function merges two cleaned dataframes.
#'
#' @param df1 The first cleaned dataframe.
#' @param df2 The second cleaned dataframe.
#'
#' @return A merged dataframe.
#'
#' @examples
#' \dontrun{
#' merged_data <- merge_cleaned_datasets(cleaned_data1, cleaned_data2)
#' head(merged_data)
#' }
#'
#' @export
#' @usage merge_cleaned_datasets(df1, df2)
merge_cleaned_datasets <- function(df1, df2) {
  # Check if df1 and df2 are dataframes
  if (!is.data.frame(df1) || !is.data.frame(df2)) {
    stop("Both parameters must be data frames.")
  }

  # Merge the two dataframes using rbind
  merged_data <- rbind(df1, df2)

  return(merged_data)
}
