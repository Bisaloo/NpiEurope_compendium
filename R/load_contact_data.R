#' Load contact dataset from specific country
#'
#' @inheritParams load_country_data
#'
#' @export

load_contact_data <- function(country) {

  if (country == "Norway") {
    return(contactdata::contact_matrix("Sweden"))
  }

  if (country == "Liechtenstein") {
    return(contactdata::contact_matrix("Luxembourg"))
  }

  if (country == "United Kingdom") {
    return(contactdata::contact_matrix("UK"))
  }

  return(contactdata::contact_matrix(country))


}
