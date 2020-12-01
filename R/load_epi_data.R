#' @export
load_epi_data <- function(end_date = Sys.Date()) {

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
                          "Latvia", "Lithuania", "Luxembourg",
                          "Malta",
                          "Netherlands", "Norway",
                          "Poland", "Portugal",
                          "Romania",
                          "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
                          "United Kingdom"))

}
