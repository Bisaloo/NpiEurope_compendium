#' Get 95% credibility intervals for estimations
#'
#' @param MCMC `mcmc` or `mcmc.list` object coda object
#'
#' @importFrom utils read.csv
#'
#' @export
summarise_estimation <- function(MCMC, npi_data) {

  dataStrat <- sirage::get_strats(npi_data, min_duration = 5)

  name_strats <- rle(dataStrat)$values[-1]

  nbstrats <- length(name_strats)

  ci_params <- summary(MCMC, c(0.025, 0.975))$quantiles

  rownames(ci_params) <- c("transmRate", rle(dataStrat)$values[-1])

  resConf <- sirage::fill_confin(dataStrat, rep_len(0.5, nbstrats + 1))

  starts <- resConf$Begin
  ends <- resConf$Duration + resConf$Begin - 1

  ci_params <- data.frame(ci_params,
                          "start" = c(NA, starts[-1]),
                          "end" = c(NA, ends[-1]))

  return(ci_params)
}
