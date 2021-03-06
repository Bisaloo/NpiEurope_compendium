#' Get epidemiological data for 31 European countries
#'
#' @param end_date The latest date taken into account. (Taken as the end date
#' for strategies where no end has been registered.)
#'
#' @references
#' https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% mutate across if_else transmute relocate group_by summarise ungroup filter any_of
#' @importFrom tidyr pivot_wider
#'
#' @export
load_npi_data <- function(end_date = Sys.Date()) {

  end_date <- as.Date(end_date)

  strategies <- c(
    "StayHomeOrder", "StayHomeGen",
    "ClosDaycare", "ClosPrim", "ClosSec", "ClosHigh",
    "MassGatherAll", "MassGather50", "ClosPubAny",
    "MasksVoluntaryAllSpaces", "MasksVoluntaryClosedSpaces",
    "MasksMandatoryAllSpaces", "MasksMandatoryClosedSpaces",
    "Teleworking", "WorkplaceClosures"
  )

  strategies <- c(strategies, paste0(strategies, "Partial"))

  npi_data_location <- "https://www.ecdc.europa.eu/en/publications-data/download-data-response-measures-covid-19" %>%
    rvest::read_html() %>%
    rvest::html_elements(".download__item a") %>%
    rvest::html_attr("href") %>%
    { grep("\\.csv$", ., value = TRUE) }

  npi_data <- read.csv(npi_data_location) %>%
    filter(Response_measure %in% strategies) %>%
    mutate(Response_measure = gsub("^(Masks(Mandatory|Voluntary))((All|Closed)Spaces)(Partial)?", "\\1\\5", Response_measure)) %>%
    mutate(Response_measure = gsub("^WorkplaceClosures", "Teleworking", Response_measure)) %>%
    mutate(across(c(date_start, date_end), as.Date)) %>%
    mutate(date_end = if_else(is.na(date_end), end_date, date_end))

  holidays <- read.csv(system.file("extdata", "summer_holidays.csv", package = "NpiEurope")) %>%
    transmute(Country = country,
              date_start = as.Date(early_start),
              date_end = as.Date(early_end)) %>%
    merge(data.frame("Response_measure" = c("ClosDaycare", "ClosPrim", "ClosSec", "ClosHigh"))) %>%
    relocate(Country, Response_measure, date_start, date_end)

  npi_data <- rbind(npi_data, holidays)

  skeleton <- merge(
    data.frame("Date" = seq(as.Date("2019-12-01"), end_date, by = 1)),
    data.frame("Country" = unique(npi_data$Country))
  )

  npi_data <- merge(npi_data, skeleton) %>%
    group_by(Country) %>%
    mutate(in_use = Date >= date_start & Date <= date_end) %>%
    group_by(Country, Date, Response_measure) %>%
    summarise(in_use = any(in_use)) %>%
    pivot_wider(names_from = Response_measure, values_from = in_use) %>%
    mutate(across(!any_of(c("Country", "Date")), isTRUE)) %>%
    mutate(across(!any_of(c("Country", "Date")), as.numeric)) %>%
    ungroup() %>%
    filter(Date <= end_date)

  return(npi_data)
}
