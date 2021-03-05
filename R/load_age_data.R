#' @importFrom contactdata age_df_countries
#' @importFrom dplyr %>% mutate group_by summarise ungroup
load_age_data <- function(country) {

  if (country == "Norway") {
    age_data <- age_df_countries("Sweden")
  } else if (country == "Liechtenstein") {
    age_data <- age_df_countries("Luxembourg")
  } else if (country == "United Kingdom") {
    age_data <- age_df_countries("UK")
  } else {
    age_data <- age_df_countries(country)
  }

  age_data %>% mutate(
    age = forcats::fct_collapse(
      age,
      "0_9" = c("0_4", "5_9"),
      "10_19" = c("10_14", "15_19"),
      "20_29" = c("20_24", "25_29"),
      "30_39" = c("30_34", "35_39"),
      "40_49" = c("40_44", "45_49"),
      "50_59" = c("50_54", "55_59"),
      "60_69" = c("60_64", "65_69"),
      other_level = "70+"
    )
  ) %>%
    group_by(age) %>%
    summarise(population = sum(population)) %>%
    ungroup()

}
