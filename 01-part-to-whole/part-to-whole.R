library(readr)
library(dplyr)
library(forcats)
library(ggplot2)
library(waffle)

march_time_tracking <- read_csv(here::here("01-part-to-whole", "march_time_tracking.csv"))

march_time_tracking <- march_time_tracking %>%
  mutate(category = fct_reorder(category, hours_percent_rounded)) %>%
  arrange(category) %>%
  mutate(colour = c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"))

category_labels <- tribble(
  ~category, ~x, ~y, ~angle,
  "Admin", 0, 1.5, 90,
  "Prospective work", 0, 4.5, 90,
  "Community", 0, 8.5, 90,
  "Personal projects", 3.5, 11, 0,
  "Client work", 8, 11, 0
) %>%
  left_join(march_time_tracking %>%
    select(category, colour), by = "category")

ggplot() +
  geom_waffle(data = march_time_tracking, aes(fill = colour, values = hours_percent_rounded), colour = "white") +
  geom_text(data = category_labels, aes(x = x, y = y, label = category, angle = angle, colour = colour)) +
  geom_text(aes(x = 1.5, y = 0, label = "1 square is 1% of tracked time"), hjust = 0, size = 3, colour = "#264653") +
  geom_curve(aes(x = 1.45, xend = 1, y = 0, yend = 0.4), curvature = -0.25, size = 0.25, colour = "#264653", arrow = arrow(length = unit(0.01, "npc"))) +
  scale_colour_identity() +
  scale_fill_identity() +
  coord_fixed() +
  theme_void() +
  theme(legend.position = "none")

ggsave(here::here("01-part-to-whole", "part-to-whole.png"), width = 7, height = 4, dpi = 500)
