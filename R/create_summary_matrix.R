#' @importFrom purrr map_dfr
#'
#' @export
create_summary_matrix <- function(folder, date = Sys.Date()) {

  npi_europe <- load_npi_data(date)

  list.files(folder, pattern = "\\.csv$", full.names = TRUE) %>%
    map_dfr(create_summary_country, npi_europe)

}

#' @importFrom sirage summarise_estimation
create_summary_country <- function(file, npi_europe) {

  country <- gsub(".*/((\\w|\\s)+)\\.csv$", "\\1", file)

  npi_country <- npi_europe %>%
    filter(Country == country) %>%
    select(-Country)

  contact_country <- load_contact_data(country)
  age_country <- sirage::load_age_data(country)

  s <- summarise_estimation(file, npi_country, contact_country, age_country)[-1, ] %>%
    rownames_to_column("strats")

  skeleton <- setNames(
    as.data.frame(matrix(0L, ncol = ncol(npi_country) - 1, nrow = nrow(s))),
    colnames(npi_country)[-1]
  )

  cbind(s, skeleton) %>%
    mutate(across(!any_of(c("strats", "X2.5.", "X97.5.", "start", "end")),
                  function(x) as.numeric(grepl(paste0(cur_column(), "\\b"), strats)))) %>%
    select(-strats) %>%
    rowwise() %>%
    mutate(NbStrategies = sum(c_across(!any_of(c("strats", "X2.5.", "X97.5.", "start", "end"))))) %>%
    filter(NbStrategies != 0) %>%
    mutate(
      Efficiency = (X2.5. + X97.5.) /2,
      DayStart = start,
      DayEnd = end,
      Country = country,
      duration = end - start,
      .keep = "unused"
    )

}
