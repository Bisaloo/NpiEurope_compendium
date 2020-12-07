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
    return(contact_matrix("Sweden"))
  }

  if (country == "Liechtenstein") {
    return(contact_matrix("Luxembourg"))
  }

  if (country == "United Kingdom") {
    return(contact_matrix("UK"))
  }

  return(contact_matrix(country))


}
