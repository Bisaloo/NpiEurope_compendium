#' Get epidemiological data for 31 European countries
#'
#' @inheritParams load_npi_data
#'
#' @references
#' https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% filter transmute arrange
#'
#' @export
load_epi_data <- function(end_date = Sys.Date()){

  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv") %>%
    filter(continentExp == "Europe") %>%
    transmute(Date = as.Date(paste(year, month, day, sep = "-")),
              NewCases = cases,
              NewDeaths = deaths,
              Country = countriesAndTerritories) %>%
    filter(Country %in% c("Austria",
                          "Belgium", "Bulgaria",
                          "Croatia", "Cyprus", "Czechia",
                          "Denmark",
                          "Estonia",
                          "Finland", "France",
                          "Germany", "Greece",
                          "Hungary",
                          "Iceland", "Ireland", "Italy",
                          "Latvia", "Liechtenstein", "Lithuania", "Luxembourg",
                          "Malta",
                          "Netherlands", "Norway",
                          "Poland", "Portugal",
                          "Romania",
                          "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                          "United_Kingdom")) %>%
    mutate(Country = if_else(Country == "United_Kingdom", "United Kingdom", Country)) %>%
    filter(Date <= end_date) %>%
    group_by(Country) %>%
    filter(cumsum(NewCases) > 0) %>%
    ungroup() %>%
    arrange(Country, Date)

}
