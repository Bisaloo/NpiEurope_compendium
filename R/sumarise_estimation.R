#' Get 95% credibility intervals for estimations
#'
#' @importFrom utils read.csv
#'
#' @export
summarise_estimation <- function(MCMC_file, npi_data, contact_data, age_data) {

  if (!file.exists(MCMC_file)) {
      stop(
        "You specified a file path but no file was found there. ",
        "Please try again.", call. = FALSE
      )
  }

  pMatrix <- matrix(NA_real_, ncol = 8, nrow = 8)
  for (i in 1:8) {
    for (j in 1:8) {
      pMatrix[i, j] <- sum(contact_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c((j - 1) * 2 + 1, (j - 1) * 2 + 2)])
    }
  }

  popAge <- rep(0, 8)
  for (i in 1:8) {
    popAge[i] <- sum(age_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), "Pop"])
  }
  propAge <- popAge / sum(popAge)

  dataStrat <- sirage::get_strats(npi_data, min_duration = 5)

  name_strats <- rle(dataStrat)$values[-1]

  nbstrats <- length(name_strats)

  # Fixed parameters
  epsilon <- 1 / 3
  sigma <- 1 / 5

  df <- as.matrix(read.csv(MCMC_file))
  lkl <- df[, 2]
  df <- df[, 3:ncol(df), drop = FALSE]
  df[, 1] <- rowSums(tcrossprod(df[, 1], popAge * rowSums(pMatrix))) / (epsilon + sigma)
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
