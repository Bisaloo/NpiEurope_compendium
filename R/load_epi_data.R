load_epi_data <- function() {

  read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv/data.csv") %>%
    filter(continentExp == "Europe") %>%
    transmute(Date = as.Date(paste(year, month, day, sep = "-")),
              NewCases = cases,
              NewDeaths = deaths,
              Country = countriesAndTerritories)

}
