#' Load and Clean Storm Dataset from Package
#'
#' This function demonstrates how to load and clean a storm dataset from a CSV file
#' located in the 'inst' folder of the package.
#'
#' @param file_name A character string specifying the name of the CSV file (including extension)
#'                 located in the 'inst' folder of the package.
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#'
#' @return A cleaned and transformed data frame of the storm dataset.
#'
#' @examples
#' \dontrun{
#' cleaned_storm_data <- load_and_clean_storm_data(file_name = "storm_2013.csv")
#' head(cleaned_storm_data)
#' }
#'
#' @export
#' @importFrom utils read.csv
#' @importFrom dplyr %>%
#' @usage load_and_clean_storm_data(file_name)
load_and_clean_storm_data <- function(file_name) {
  # Load the storm dataset from the 'inst' folder of the package
  dataset_path <- system.file("inst", file_name, package = "StormDataAnalysis")
  storm_data <- read.csv(dataset_path)

  # Data cleaning steps
  # Replace missing values with NA
  storm_data <- dplyr::na_if(storm_data, "")

  # Convert date and time columns to appropriate formats
  storm_data$date <- as.Date(storm_data$date, format = "%Y-%m-%d")
  storm_data$time <- as.POSIXct(storm_data$time, format = "%H:%M:%S")

  # Remove duplicate rows
  storm_data <- dplyr::distinct(storm_data)

  # Remove columns with all missing values (NA)
  storm_data <- storm_data %>%
    dplyr::select_if(~!all(is.na(.)))

  return(storm_data)
}
