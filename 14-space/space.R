library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(purrr)
library(forcats)
library(trekcolors)
library(ggstar)
library(showtext)

font <- "Khan"
path <- system.file(paste0("fonts/", font, ".ttf"), package = "trekfont")
for(i in seq_along(font)) font_add(font[i], path[i])
showtext_auto()

lautaro <- read_csv(here::here("14-space", "lautaro.csv"))
sharla <- read_csv(here::here("14-space", "sharla.csv")) %>%
  mutate(Date = mdy(Date))

viewing_history <- lautaro %>%
  bind_rows(sharla)

tng_episodes <- viewing_history %>%
  filter(str_detect(Title, "Star Trek: The Next Generation:")) %>%
  separate(Title, into = c("show", "season"), sep = "Season ") %>%
  separate(season, into = c("season", "episode"), sep = ": ", convert = TRUE) %>%
  select(season, episode, date = Date) %>%
  filter(!(season == 1 & date == "2021-04-10") & # We went back to see how weird everyone looked in the first season this weekend lol
    !(season == 4 & date == "2021-03-20")) # Idk why we went back to "First Contact" but must have checked something

tng_season_watch_dates <- tng_episodes %>%
  group_by(season) %>%
  summarise(
    start_date = min(date),
    finish_date = max(date),
    days = as.numeric(finish_date - start_date)
  ) %>%
  mutate(
    size = ifelse(season != 7, 5, NA),
    season_chr = glue::glue("Season {season}"),
    season_chr = fct_reorder(season_chr, -season),
    season = -season
  ) %>%
  select(season, season_chr, start_date, finish_date, days, size)

season_7_annotation <- tng_season_watch_dates %>%
  filter(season_chr == "Season 7") %>%
  mutate(annotation = "Still\ngoing!")

ggplot(tng_season_watch_dates, aes(color = season_chr, fill = season_chr)) +
  geom_segment(aes(x = start_date, xend = finish_date, y = season, yend = season), size = 2, lineend = "round") +
  geom_star(aes(x = start_date, y = season, starshape = "star"), size = 5) +
  geom_star(aes(x = finish_date, y = season, starshape = "star", size = size)) +
  scale_starshape_manual(9) +
  scale_size_identity() +
  geom_text(aes(x = start_date - 7, y = season, label = season_chr), family = "Khan", size = 5) +
  geom_text(data = season_7_annotation, aes(x = finish_date + 3, y = season, label = annotation), family = "Khan", size = 3, vjust = -1, lineheight = 0.75, color = "#f605a9") +
  geom_curve(data = season_7_annotation, aes(x = finish_date + 2.75, xend = finish_date + 0.75, y = season + 0.2, yend = season), curvature = -0.25, color = "#f605a9", lineend = "round", arrow = arrow(length = unit(0.05, "inches"), type = "closed")) +
  scale_x_date(date_labels = "%b %d", breaks = as.Date(c("2021-01-11", "2021-01-25", "2021-02-08", "2021-02-22", "2021-03-08", "2021-03-22", "2021-04-05"))) +
  labs(x = NULL, y = NULL, title = "Star Trek: The Next Generation watching history") +
  scale_color_trek("lcars_2369") +
  scale_fill_trek("lcars_2369") +
  theme_grey(base_size = 20, base_family = "Khan") +
  theme(
    plot.background = element_rect(fill = "#2a0660", color = "#2a0660"),
    panel.background = element_rect(fill = "#2a0660"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#4200a1"),
    legend.position = "none",
    axis.text = element_text(colour = "#08b5f8"),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(color = "#f605a9", family = "Khan", hjust = 0.5)
  )

ggsave(here::here("14-space", "space.png"), width = 10.5, height = 6, dpi = 300)

data <- tng_season_watch_dates %>%
  select(season = season_chr, start_date, finish_date, days)

write_csv(data, here::here("14-space", "data.csv"))
