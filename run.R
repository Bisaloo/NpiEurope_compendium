rm(list=ls())

remotes::install_github("Bisaloo/sir_age", upgrade = TRUE, force = TRUE)
library(sirage)

remotes::install_github("Bisaloo/Npieurope", upgrade = TRUE)
library(NpiEurope)

folder <- paste0("/scratch/gruson-", Sys.getenv("$SLURM_JOB_ID", Sys.Date()))
dir.create(folder)
setwd(folder)

library(foreach)
library(dplyr)

cl <- parallel::makeForkCluster(4)
doParallel::registerDoParallel(cl)

temp <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-09-16.csv", package = "NpiEurope"),
                 stringsAsFactors = FALSE)
countryVec <- unique(temp$Country)

foreach(country=countryVec) %dopar% {

  message(country)

  country_data <- NpiEurope::load_country_data(country)
  contact_data <- NpiEurope::load_contact_data(country)
  age_data <- load_age_data(country)
  epi_data <- country_data[, c("Date", "NewCases", "NewDeaths")]
  npi_data <- country_data[, !colnames(country_data) %in% c("NewCases", "NewDeaths", "Country", "Population")]

  epi_data <- epi_data %>%
    estimate_asympto(smooth = 5)

  res <- simulate_country(epi_data, npi_data, contact_data, age_data, task = "estimate",
                          Np = 50, Niter = 5000, outfile = country)
  saveRDS(res, sprintf("fullmcmc_%s.rds", country))

}

parallel::stopCluster(cl)
