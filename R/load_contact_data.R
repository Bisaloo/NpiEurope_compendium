#' @importFrom contactdata contact_matrix
#'
#' @export

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
