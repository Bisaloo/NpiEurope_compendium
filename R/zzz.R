.onLoad <- function(libname, pkgname) {

  # Memoise functions to reduce load on ECDC servers
  load_npi_data <<- memoise::memoise(load_npi_data)
  load_epi_data <<- memoise::memoise(load_epi_data)

}

