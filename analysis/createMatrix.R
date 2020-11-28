library(ggplot2)

remotes::install_github("Bisaloo/sir_age", upgrade = TRUE)
library(sirage)

remotes::install_github("Bisaloo/Npieurope", upgrade = TRUE)
library(NpiEurope)

temp <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-06-26.csv", package = "NpiEurope"),
  stringsAsFactors = FALSE
)
countryVec <- sort(unique(temp$Country))
resEff <- c()
resCountry <- c()
resStrat <- c()

folder <- "MCMCOK5"

# From IMF, Wikipedia (except for liechentenstien, WB)
gdp <- c(
  50222, 45175, 9518, 14949, 27719, 23313, 59795, 23523, 48868, 41760, 46563,
  19974, 17463, 67037, 77771, 32946, 18171, 173356, 19266, 113196, 30650,
  52367, 77975, 14901, 23030, 12482, 19547, 26170, 29961, 51241, 83716, 41030
)
for (i in 1:length(countryVec)) {
  country <- countryVec[i]
  print(country)
  country_data <- load_country_data(country)
  country_data$NewCases[which(country_data$NewCases < 0)] <- -country_data$NewCases[which(country_data$NewCases < 0)]
  epi_data <- country_data[, c("Date", "NewCases", "NewDeaths")]
  npi_data <- country_data[, !colnames(country_data) %in% c("NewCases", "NewDeaths", "Country", "Population")]
  epi_data <- estimate_asympto(epi_data, smooth = 7)
  contact_data <- NpiEurope::load_contact_data(country)
  age_data <- load_age_data(country)

  plot_asympto(epi_data)
  ggsave(sprintf("%s/Report_%s.pdf", folder, country))
  temp <- summarise_estimation(sprintf("%s/%s.csv", folder, country), npi_data, contact_data, age_data)
  write.csv(temp, sprintf("%s/estim_%s.csv", folder, country))
  # write.csv(summarise_estimation(sprintf("%s/MCMC_%s.csv",folder,country), country_data,contact_data,age_data),sprintf("%s/estim_%s.csv",folder,country))
  for (j in 2:length(temp[, 1])) {
    tempNew <- unlist(strsplit(row.names(temp[j, ]), " "))
    for (k in 1:length(tempNew)) {
      resEff <- c(resEff, (temp[j, 1] + temp[j, 2]) / 2)
      resCountry <- c(resCountry, country)
      resStrat <- c(resStrat, tempNew[k])
    }
  }
}

stratUn <- unique(resStrat)
matResults <- c()
for (i in 1:length(countryVec)) {
  country <- countryVec[i]
  print(country)
  country_data <- load_country_data(country)
  country_data$NewCases[which(country_data$NewCases < 0)] <- -country_data$NewCases[which(country_data$NewCases < 0)]
  epi_data <- country_data[, c("Date", "NewCases", "NewDeaths")]
  npi_data <- country_data[, !colnames(country_data) %in% c("NewCases", "NewDeaths", "Country", "Population")]
  epi_data <- estimate_asympto(epi_data, smooth = 7)
  contact_data <- NpiEurope::load_contact_data(country)
  age_data <- load_age_data(country)

  res <- summarise_estimation(sprintf("%s/%s.csv", folder, country), npi_data, contact_data, age_data)

  for (j in 2:length(res[, 1])) {
    temp <- rep(0, length(stratUn) + 6)
    tempNew <- unlist(strsplit(row.names(res[j, ]), " "))
    for (k in 1:length(tempNew)) {
      temp[which(stratUn == tempNew[k])] <- 1
      # resEff<-c(resEff,(temp[j,1]+temp[j,2])/2);
      # resCountry<-c(resCountry,country)
      # resStrat<-c(resStrat,tempNew[k]);
    }
    temp[length(stratUn) + 1] <- (res[j, 1] + res[j, 2]) / 2
    temp[length(stratUn) + 2] <- res[j, 3]
    temp[length(stratUn) + 3] <- res[j, 4]
    temp[length(stratUn) + 4] <- gdp[i]
    temp[length(stratUn) + 5] <- i
    temp[length(stratUn) + 6] <- length(tempNew)
    matResults <- rbind(matResults, temp)
  }
}

combStrat <- c()
for (i in seq_len(nrow(matResults))) {
  combStrat <- c(combStrat, sum(matResults[i, 1:24]))
}
colnames(matResults) <- c(stratUn, "Efficiency", "DayStart", "DayEnd", "GDP", "CountryCode", "NbStrategies")
save(matResults, file = "matResults.RData")
