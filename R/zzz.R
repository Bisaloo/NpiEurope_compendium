.onLoad <- function(libname, pkgname) {

  # Memoise functions to reduce load on ECDC servers
  load_npi_data <<- memoise::memoise(load_npi_data)
  load_epi_data <<- memoise::memoise(load_epi_data)

  # Memoise slow function
  create_summary_matrix <<- memoise::memoise(create_summary_matrix)

  # Fix warnings due to non-standard evaluation
  # utils::globalVariables(
  #   c("Country", "strats", "NbStrategies", "X2.5.", "X97.5.", "start", "end")
  # )

}

