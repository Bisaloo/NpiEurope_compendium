#' @inherit contactdata::contact_matrix
#'
#' @importFrom contactdata contact_matrix
#'
#' @examples
#' oad_contact_data("France")
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
