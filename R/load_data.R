#' Load CSV Data from Package
#'
#' This function loads data in CSV format from the 'inst' folder of the package.
#'
#' @param file_name A character string specifying the name of the CSV file (including extension)
#'
#' @importFrom utils read.csv
#'
#' @return A data frame containing the contents of the loaded CSV file.
#'
#' @examples
#' \dontrun{
#' loaded_data <- load_csv_from_package(file_name = "data_file.csv")
#' head(loaded_data)
#' }
#'
#' @export
#' @importFrom utils read.csv
#' @usage load_csv_from_package(file_name)
load_csv_from_package <- function(file_name) {
  # Load the CSV file from the 'inst' folder of the package
  dataset_path <- system.file(file_name, package = "StormDataAnalysis")
  loaded_data <- read.csv(dataset_path)

  return(loaded_data)
}
