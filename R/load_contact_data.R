#' Load contact dataset from specific country
#'
#' @inheritParams load_country_data
#'
#' @importFrom readxl read_xlsx
#'
#' @export

load_contact_data <- function(country) {

  contact_data <- read_xlsx(
    system.file("extdata", "ctc.xlsx", package = "NpiEurope"),
    col_names = FALSE,
    sheet = country
  )

  return(contact_data)
}
