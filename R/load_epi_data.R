#' Get epidemiological data for 31 European countries
#'
#' @references
#' https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% filter transmute arrange
#'
#' @export
load_epi_data <- function() {

  read.csv(system.file("extdata", "epi_europe.csv", package = "NpiEurope"),
           colClasses = c("Date", "integer", "integer", "character"))

}
