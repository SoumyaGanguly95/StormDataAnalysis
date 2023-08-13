#' Exploratory Data Analysis for Storm Dataset
#'
#' This function performs exploratory data analysis on the cleaned storm dataset,
#' providing a count of different storm categories, a comparative analysis of their durations,
#' and counts of event types.
#'
#' @param storm_data A cleaned and transformed data frame of the storm dataset.
#'
#' @return A summary of storm counts, comparative analysis of storm durations,
#' and event type counts.
#'
#' @examples
#' \dontrun{
#' cleaned_storm_data <- load_and_clean_storm_data() # Load and clean your data
#' eda_results <- explore_storm_data(cleaned_storm_data)
#' print(eda_results)
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr count summarize
#' @export
explore_storm_data <- function(storm_data) {
  # Calculate storm counts by category
  storm_counts <- dplyr::count(storm_data, magnitude_type)

  # Calculate mean duration for each storm category
  mean_durations <- storm_data %>%
    dplyr::group_by(magnitude_type) %>%
    dplyr::summarize(mean_duration_of_storm = mean(storm_duration, na.rm = TRUE))

  # Calculate event type counts
  event_type_counts <- dplyr::count(storm_data, event_type)

  # Perform comparative analysis of durations
  duration_comparison <- mean_durations %>%
    dplyr::mutate(mean_storm_duration_in_hours = mean_duration_of_storm / (60 * 60))

  return(list(storm_counts = storm_counts,
              duration_comparison = duration_comparison,
              event_type_counts = event_type_counts))
}
