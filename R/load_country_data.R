#' Load and clean up dataset from specific country
#'
#' @param country Character string. Name of the country for which you want to
#' run this analysis
#' @param dataset_path Path of the dataset with the epidemiological data for
#' all countries
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter %>% across mutate
#'
#' @export
load_country_data <- function(country, dataset_path = NULL) {
  if (length(country) != 1) {
    stop("Processing of multiple countries at once is not yet implemented. ",
      "Please provide a single country name in 'country'",
      call. = FALSE
    )
  }

  if (is.null(dataset_path)) {
    dataset_path <- system.file("extdata", "COVID_time_series_v4_2020-09-16.csv", package = "NpiEurope")
  }

  country_data <- read_csv(dataset_path) %>%
    filter(Country == country) %>%
    filter(cumsum(NewCases) > 0) %>%
    filter(rev(cumsum(rev(NewCases))) > 0) %>%
    filter(Date <= "2020-09-08") %>%
    as.data.frame()

  holidays <- read_csv(system.file("extdata", "summer_holidays.csv", package = "NpiEurope")) %>%
    mutate(across(!country, as.Date, format = "%d-%m-%Y")) %>%
    mutate(across(!country, format, format = "%Y-%m-%d")) %>%
    mutate(across(!country, as.Date, format = "%Y-%m-%d")) %>%
    filter(country == !!country)

  country_data[country_data$Date >= holidays$early_start & country_data$Date <= holidays$early_end, c("ClosDaycare", "ClosPrim", "ClosSec", "ClosHigh")] <- 1

  if (nrow(country_data) == 0) {
    stop("This country is not included in our dataset. ",
      "Please double check spelling",
      call. = FALSE
    )
  }

  return(country_data)
}
