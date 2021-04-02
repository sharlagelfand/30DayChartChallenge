library(repurrrsive)
library(tidyr)
library(dplyr)
library(stringr)

# repurrrsive::discog contains my music collection from discogs as of a few years ago. Isn't that cool :sob:
# It's super nested/messy though lol

collection <- tibble(disc = discog) %>%
  unnest_wider(disc) %>%
  select(!id) %>%
  unnest_wider(basic_information) %>%
  select(formats) %>%
  mutate(id = row_number()) %>% # Just to keep track of actual #s in case 1 row becomes many
  unnest_longer(formats) %>%
  unnest_wider(formats) %>%
  group_by(id, name) %>%
  summarise(
    descriptions = paste0(unlist(descriptions), collapse = ", "),
    .groups = "drop"
  )

collection <- collection %>%
  mutate(category = case_when(
    name == "Vinyl" & str_detect(descriptions, "7") ~ '7"',
    name == "Vinyl" & (str_detect(descriptions, "LP") | str_detect(descriptions, "12")) ~ '12"',
    name == "Cassette" ~ "Tape"
  )) %>%
  count(category)

readr::write_csv(collection, here::here("02-pictogram", "music_collection.csv"))
