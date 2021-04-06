library(flametree)
library(dplyr)
library(readr)
library(ggplot2)

forest_area <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv")

canada_forest_area <- forest_area %>%
  filter(entity == "Canada")

canada_forest_area_birth_year <- canada_forest_area %>%
  filter(year == 1991) %>%
  pull(forest_area)

data <- flametree_grow(
  # Using my birthday as data lol
  seed = 1991,
  split = 6 / 2,
  time = 24 / 3,
  scale = canada_forest_area[["forest_area"]] / 10,
  prune = canada_forest_area_birth_year / 1000
)

flametree <- data %>%
  flametree_plot(
    background = "#1D4246",
    palette = "scico::bamako"
  )

ggsave(here::here("06-experimental", "experimental.png"), flametree, height = 4, width = 6)
