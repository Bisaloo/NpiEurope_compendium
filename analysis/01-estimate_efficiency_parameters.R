library(dplyr)

library(sirage)
library(NpiEurope)

folder <- "MCMC_NpiEurope"
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
    mutate(NewCases = pmax(NewCases, 0),
           NewDeaths = pmax(NewDeaths, 0)) %>%
    merge(asymptor::estimate_asympto(.$Date, .$NewCases, .$NewDeaths, bounds = "lower")) %>%
    mutate(PropAsympto = lower / (lower+new_cases)) %>%
    mutate(PropAsympto = ifelse(is.finite(PropAsympto), PropAsympto, 0)) %>%
    mutate(PropAsympto = slider::slide_dbl(PropAsympto, mean, .before = 3, .after = 3, .complete = FALSE))

  npi_data <- load_npi_data() %>%
    filter(Country == country) %>%
    select(-Country)

  oldchain <- read.csv(sprintf("%s/%s.csv", folder, country))
  transmRate0 <- oldchain$transmRate[nrow(oldchain)]
  vecEff0 <-  unlist(oldchain[nrow(oldchain), grepl("^vecEff", colnames(oldchain))])

  res <- simulate_country(epi_data, npi_data, contact_data, age_data,
                          task = "estimate",
                          Np = 10, Niter = 50,
                          transmRate = transmRate0, vecEff = vecEff0,
                          outfile = paste0(folder, "/", country))

  saveRDS(res, sprintf("%s/fullmcmc_%s.rds", folder, country))

}
