#-------------------------------------------------
# Summary of public health responses efficiencies
#-------------------------------------------------

remotes::install_github("Bisaloo/Npieurope", upgrade = TRUE)
library(NpiEurope)

library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)

npi_europe <- read.csv(system.file("extdata", "COVID_time_series_v4_2020-09-16.csv", package = "NpiEurope"),
         stringsAsFactors = FALSE) %>%
  filter(Date <= "2020-09-08") %>%
  select(!any_of(c("NewCases", "NewDeaths", "Population", "Date")))

(
duration_plot <- npi_europe %>%
  group_by(Country) %>%
  summarise(across(everything(), sum)) %>%
  select(-Country) %>%
  map_dfc(function(x) c(median(x[x != 0]), sum(x != 0))) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("NPI") %>%
  rename(Duration = V1,
         Implemented = V2) %>%
  mutate(Implemented = Implemented / 32 * 100) %>%
  ggplot(aes(x = Implemented, y = NPI, fill = Duration)) +
    geom_bar(stat = "identity") +
    theme_bw() +
    labs(x = "% Countries Implemented",
         y = "",
         fill = "Median duration (days)")
)

library(ggcorrplot)

(
correlation_plot <- npi_europe %>%
  select(-Country) %>%
  relocate(sort(tidyselect::peek_vars())) %>%
  cor() %>%
  ggcorrplot(method = "circle", type = "upper",
             hc.order = FALSE,
             colors = c("tomato2", "white", "springgreen3"),
             ggtheme = theme_bw,
             legend.title = "Correlation")
)

library(patchwork)

duration_plot + correlation_plot +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "top")

ggsave("Figure 1.pdf", width = 29.7, height = 21, units = "cm")
