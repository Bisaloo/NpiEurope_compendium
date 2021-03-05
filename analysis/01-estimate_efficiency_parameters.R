library(dplyr)

devtools::load_all()

oldfolder <- "goodstart"

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

  age_data <- load_age_data(country) %>%
    mutate(
      age = forcats::fct_collapse(
        age,
        "0_9" = c("0_4", "5_9"),
        "10_19" = c("10_14", "15_19"),
        "20_29" = c("20_24", "25_29"),
        "30_39" = c("30_34", "35_39"),
        "40_49" = c("40_44", "45_49"),
        "50_59" = c("50_54", "55_59"),
        "60_69" = c("60_64", "65_69"),
        other_level = "70+"
      )
    ) %>%
    group_by(age) %>%
    summarise(population = sum(population)) %>%
    pull(population)

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

  P0 <- data.frame(
    S = floor(rep_len(1, 8) * age_data),
    E = rep_len(0, 8),
    A = rep_len(1, 8),
    I = rep_len(1, 8),
    R = rep_len(0, 8),
    U = rep_len(0, 8),
    D = rep_len(0, 8),
    M = rep_len(0, 8),
    DD = rep_len(0, 8),
    V = rep_len(0, 8)
  )

  dataStrat <- npi_data %>%
    select(-Date) %>%
    apply(1, function(s) paste(colnames(.)[as.logical(s)], collapse = " "))

  nbstrats <- length(rle(dataStrat)$values)

  tchanges <- cumsum(rle(dataStrat)$lengths) + 1

  oldchain <- read.csv(sprintf("%s/%s.csv", oldfolder, country))
  transmRate0 <- unlist(oldchain[which.max(oldchain$Likelihood), startsWith(names(oldchain), "transm")])

  if (length(transmRate0) < nbstrats) {
    transmRate0 <- c(transmRate0, rep_len(mean(transmRate0), length(transmRate0) - nbstrats))
  } else if (length(transmRate0) > nbstrats) {
    transmRate0 <- transmRate0[seq_len(nbstrats)]
  }

  names(transmRate0) <- paste0("transm", seq_along(transmRate0))

  susc_rates <- setNames(
    unlist(oldchain[which.max(oldchain$Likelihood), startsWith(names(oldchain), "susc")]),
    paste0("susc", seq_len(8))
  )

  taus <- setNames(
    unlist(oldchain[which.max(oldchain$Likelihood), startsWith(names(oldchain), "tau")]),
    paste0("tau", seq_len(2))
  )

  death_delay <- setNames(
    unlist(oldchain[which.max(oldchain$Likelihood), startsWith(names(oldchain), "death")]),
    "death_delay"
  )

  vcv <- matrix(0, nrow = nbstrats + 8 + 2 + 1, ncol = nbstrats + 8 + 2 + 1)

  diag(vcv) <- c(rep_len(1e-18, nbstrats), rep_len(0.01, 8), rep_len(0.01, 2), 0.25)

  resmc <- simpleMH::simpleMH(
    sirmodels::SEIR_age_get_lkl,
    inits = c(transmRate0, susc_rates, taus, death_delay),
    theta.cov = vcv,
    max.iter = 10,
    coda = TRUE,
    obs = epi_data,
    t = c(0, seq_along(epi_data$Date)),
    Np = 1,
    P0 = P0,
    t_changes = tchanges,
    prop_asympto = epi_data$PropAsympto,
    contact_matrix = contact_data,
    lethality_age = c(0.01, 0, 0.01, 0.02, 0.05, 0.11, 0.24, 0.4),
    severity_age = rep_len(0, 8L),
    rho = 0,
    vaccine_eff = 0,
    vaccine_start = 0
  )

  saveRDS(resmc, sprintf("%s/fullmcmc2_%s.rds", resfolder, state))

}
