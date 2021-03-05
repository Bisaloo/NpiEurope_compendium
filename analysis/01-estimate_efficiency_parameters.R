library(dplyr)

library(sirage)
library(NpiEurope)


resfolder <- paste("weekly_transmRate", Sys.Date(), stringi::stri_rand_strings(1, 10), sep = "_")
dir.create(resfolder)

countries <- c("Austria",
               "Belgium", "Bulgaria",
               "Croatia", "Cyprus", "Czechia",
               "Denmark",
               "Estonia",
               "Finland", "France",
               "Germany", "Greece",
               "Hungary",
               "Iceland", "Ireland", "Italy",
               "Latvia", "Lithuania", "Liechtenstein", "Luxembourg",
               "Malta",
               "Netherlands", "Norway",
               "Poland", "Portugal",
               "Romania",
               "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
               "United Kingdom")

library(foreach)
library(doFuture)
# To make sure this works fine on all machines, we don't force a parallelization
# strategy here. The user should chose it either by running future::plan() or
# by setting the R_FUTURE_PLAN env variable.
registerDoFuture()

foreach (country=countries) %dopar% {

  message(country)

  contact_data <- load_contact_data(country)
  age_data <- load_age_data(country)
  epi_data <- covidregionaldata::get_national_data(country, source = "who") %>%
    select(date, cases_new, deaths_new) %>%
    mutate(
      Date = date,
      NewCases = pmax(cases_new, 0),
      NewDeaths = pmax(deaths_new, 0),
      .keep = "unused"
    ) %>%
    filter(cumsum(NewCases) > 0) %>%
    mutate(lower = asymptor::estimate_asympto(Date, NewCases, NewDeaths, bounds = "lower")$lower) %>%
    mutate(PropAsympto = lower / (lower+NewCases)) %>%
    mutate(PropAsympto = ifelse(is.finite(PropAsympto), PropAsympto, 0)) %>%
    mutate(PropAsympto = slider::slide_dbl(PropAsympto, mean, .before = 1, .after = 1, .complete = FALSE))

  npi_data <- load_npi_data() %>%
    filter(Country == country) %>%
    select(-Country) %>%
    filter(Date %in% epi_data$Date)

  oldchain <- read.csv(sprintf("%s/%s.csv", oldfolder, country))
  transmRates0 <-  unlist(oldchain[which.max(oldchain$Likelihood), grepl("^transmRate", colnames(oldchain))])

  res <- simulate_country(epi_data, npi_data, contact_data, age_data,
                          task = "estimate",
                          Np = 10, Niter = 1000,
                          transmRates = transmRates0,
                          outfile = paste0(resfolder, "/", country),
                          min_npi_duration = 5)

  saveRDS(res, sprintf("%s/fullmcmc_%s.rds", resfolder, country))

}
