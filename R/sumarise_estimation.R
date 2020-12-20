#' Get 95% credibility intervals for estimations
#'
#' @importFrom utils read.csv
#'
#' @export
summarise_estimation <- function(MCMC, npi_data) {

  if (!file.exists(MCMC_file)) {
      stop(
        "You specified a file path but no file was found there. ",
        "Please try again.", call. = FALSE
      )
  }


  dataStrat <- sirage::get_strats(npi_data, min_duration = 5)

  name_strats <- rle(dataStrat)$values[-1]

  nbstrats <- length(name_strats)

  df <- as.matrix(read.csv(MCMC_file))
  lkl <- df[, 2]
  df <- df[, 3:ncol(df), drop = FALSE]
  ci_params <- t(apply(df, 2, quantile, c(0.025, 0.975), na.rm  = TRUE))

  rownames(ci_params) <- c("R0", rle(dataStrat)$values[-1])

  resConf <- sirage::fill_confin(dataStrat, rep_len(0.5, nbstrats + 1))

  starts <- resConf$Begin
  ends <- resConf$Duration + resConf$Begin - 1

  ci_params <- data.frame(ci_params,
                          "start" = c(NA, starts[-1]),
                          "end" = c(NA, ends[-1]))

  return(ci_params)
}
