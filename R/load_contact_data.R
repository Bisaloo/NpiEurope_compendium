#' @inherit contactdata::contact_matrix
#'
#' @importFrom contactdata contact_matrix
#'
#' @examples
#' load_contact_data("France")
#'
#' @export
#'
#' @references Kiesha Prem, Alex R. Cook, Mark Jit, Projecting social contact
#' matrices in 152 countries using contact surveys and demographic data, PLoS
#' Comp. Biol. (2017), \doi{10.1371/journal.pcbi.1005697}

load_contact_data <- function(country) {

  if (country == "Norway") {
    contact_data <- contact_matrix("Sweden")
  } else if (country == "Liechtenstein") {
    contact_data <- contact_matrix("Luxembourg")
  } else if (country == "United Kingdom") {
    contact_data <- contact_matrix("UK")
  } else {
    contact_data <- contact_matrix(country)
  }

  contact_data2 <- matrix(NA_real_, ncol = 8, nrow = 8)
  for (i in 1:8) {
    for (j in 1:8) {
      contact_data2[i, j] <- sum(contact_data[c((i - 1) * 2 + 1, (i - 1) * 2 + 2), c((j - 1) * 2 + 1, (j - 1) * 2 + 2)])
    }
  }

  return(contact_data2)
}
