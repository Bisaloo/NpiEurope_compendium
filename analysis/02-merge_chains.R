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

  res <- list.files("allchains_casesdeaths", pattern = paste0("fullmcmc2_", c, "\\.rds$"), recursive = TRUE, full.names = TRUE) %>%
    lapply(function(x) {
      r <- readRDS(x)
      res <- cbind(r$log.p, r$samples)
      colnames(res)[1] <- "Likelihood"
      return(as.data.frame(res))
    })

  most_strats <- max(vapply(res, function(x) ncol(x), integer(1)))

  res <- res[vapply(res, function(x) nrow(x) !=0 && ncol(x) == most_strats, logical(1))]

  res <- do.call(rbind.data.frame, res)

  message(nrow(res))

  saveRDS(res, paste0("goodstart/", c, ".rds"))


}

for (c in countries) {

  message(c)

  res <- list.files("allchains_casesdeaths", pattern = paste0("fullmcmc2_", c, "\\.rds$"), recursive = TRUE, full.names = TRUE) %>%
    lapply(function(x) {
      r <- readRDS(x)
      res <- cbind(r$log.p, r$samples)
      colnames(res)[1] <- "Likelihood"
      return(as.data.frame(res))
    })

  most_strats <- max(vapply(res, function(x) ncol(x), integer(1)))

  res <- res[vapply(res, function(x) nrow(x) !=0 && ncol(x) == most_strats, logical(1))]

  shortest_chain <- min(vapply(res, function(x) nrow(x), integer(1)))

  res2 <- list(
    "samples" = coda::mcmc.list(lapply(res, function(x) coda::as.mcmc(x[seq_len(shortest_chain), -1]))),
    "log.p" = list2DF(lapply(res, function(x) x$Likelihood[seq_len(shortest_chain)]))
  )

  saveRDS(res2, paste0("MCMC_NpiEurope_weekly/", c, ".rds"))

}
