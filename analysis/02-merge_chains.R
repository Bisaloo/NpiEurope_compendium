countries <- c("Austria",
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
               "United Kingdom")

library(dplyr)
library(purrr)

for (c in countries) {

  message(c)

  res <- list.files("allchains_transmRate", pattern = paste0(c, "\\.csv$"), recursive = TRUE, full.names = TRUE) %>%
    map_dfr(read.csv)

  write.csv(res, paste0("goodstart/", c, ".csv"), row.names = FALSE)

}

for (c in countries) {

  message(c)

  res <- list.files("allchains_transmRate/complete", pattern = paste0(c, "\\.csv$"), recursive = TRUE, full.names = TRUE) %>%
    lapply(function(f) {
      chain <- read.csv(f)
      list("log.p" = chain$Likelihood, "samples" = coda::as.mcmc(chain[, c(-1, -2)], start = 1, thin = 10))
    })

  res2 <- list(
    "samples" = coda::as.mcmc.list(lapply(res, function(x) x$samples)),
    "log.p" = do.call(rbind, lapply(res, function(x) x$log.p))
  )

  saveRDS(res2, paste0("MCMC_NpiEurope_weekly/", c, ".rds"))

}
