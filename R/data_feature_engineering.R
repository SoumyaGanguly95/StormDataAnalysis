#' Add Magnitude Type and Storm Duration Columns
#'
#' This function adds categorical and numerical columns 'magnitude_type' and 'storm_duration'
#' to the storm dataset based on the values in the 'magnitude' column and the difference between
#' 'event_begin_time' and 'event_end_time'.
#'
#' @param data A data frame containing the storm dataset.
#' @return The input data frame with the new columns added.
#' @examples
#' \dontrun{
#' storm_data <- load_and_clean_storm_data()
#' storm_data <- add_new_feature(storm_data)
#' head(storm_data)
#' }
#' @export
add_new_feature <- function(data) {
  # Create a categorical column 'magnitude_type' based on 'magnitude' values
  data$magnitude_type <- dplyr::case_when(
    data$magnitude < 1.0 ~ "Low",
    data$magnitude >= 1.0 & data$magnitude < 2.0 ~ "Moderate",
    data$magnitude >= 2.0 & data$magnitude < 3.0 ~ "High",
    data$magnitude >= 3.0 ~ "Severe",
    TRUE ~ NA_character_
  )

  # Calculate storm duration based on 'event_begin_time' and 'event_end_time'
  data$event_begin_time <- as.POSIXct(data$event_begin_time, format = "%Y-%m-%d %H:%M:%S")
  data$event_end_time <- as.POSIXct(data$event_end_time, format = "%Y-%m-%d %H:%M:%S")
  data$storm_duration <- difftime(data$event_end_time, data$event_begin_time, units = "mins")

  return(data)
}
