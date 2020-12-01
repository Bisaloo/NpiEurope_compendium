library(dplyr)

library(sirage)
library(NpiEurope)

folder <- paste0("MCMC_NpiEurope_", Sys.Date())
dir.create(folder)

countries <- c("Austria",
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
  epi_data <- load_epi_data() %>%
    filter(Country == country) %>%
    select(-Country) %>%
    transmute(
      date = Date,
      new_cases = NewCases,
      new_deaths = NewDeaths
    ) %>%
    mutate(new_cases = pmax(new_cases, 0),
           new_deaths = pmax(new_deaths, 0)) %>%
    merge(asymptor::estimate_asympto(., bounds = "lower")) %>%
    transmute(
      Date = date,
      NewCases = new_cases,
      PropAsympto = lower / (lower+new_cases)
    ) %>%
    mutate(PropAsympto = ifelse(is.finite(PropAsympto), PropAsympto, 0)) %>%
    mutate(PropAsympto = slider::slide_dbl(PropAsympto, mean, .before = 3, .after = 3, .complete = FALSE))

  npi_data <- load_npi_data() %>%
    filter(Country == country) %>%
    select(-Country)

  res <- simulate_country(epi_data, npi_data, contact_data, age_data, task = "estimate",
                          Np = 10, Niter = 50, outfile = paste0(folder, "/", country))
  saveRDS(res, sprintf("%s/fullmcmc_%s.rds", folder, country))

}
