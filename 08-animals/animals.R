library(opendatatoronto)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(forcats)
library(dplyr)
library(ggplot2)
library(ggchicklet)
library(ggnewscale)
library(showtext)
library(ggtext)
library(readr)

font <- "Baloo 2"
font_add_google(font)
showtext_auto()

package_resources <- list_package_resources("https://open.toronto.ca/dataset/licensed-dog-and-cat-names/") %>%
  filter(str_detect(name, "dog"))

licensed_names_raw <- map(packagBlinkere_resources[["id"]], get_resource)
names(licensed_names_raw) <- package_resources[["name"]]

licensed_names <- licensed_names_raw %>%
  map(function(x) {
    names(x) <- c("name", "n")
    x
  }) %>%
  bind_rows(.id = "source") %>%
  mutate(
    year = str_extract(source, "20.*"),
    year = parse_number(year)
  ) %>%
  select(year, name, n) %>%
  mutate(name = str_to_title(name)) %>%
  filter(!name %in% c("No Name", "No Name Listed"), !is.na(name))

top_3_names <- licensed_names %>%
  group_by(year) %>%
  top_n(3, n) %>%
  arrange(-n) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  mutate(image = here::here("08-animals", glue::glue("{name}.svg")))

rank_height <- tribble(
  ~rank, ~height,
  1, 2,
  2, 1.5,
  3, 1
)

top_3_names <- top_3_names %>%
  left_join(rank_height, by = "rank") %>%
  mutate(
    rank = as.character(rank),
    rank = fct_relevel(rank, "2", "1", "3")
  )

dog_colours <- tribble(
  ~name, ~colour,
  "Max", "#DA3E52",
  "Charlie", "#ED6A5A",
  "Buddy", "#454372",
  "Bella", "#2C3D55",
  "Luna", "#4C061D"
)

top_3_names <- top_3_names %>%
  left_join(dog_colours, by = "name")

plot_ranked_names <- function(data) {
  data %>%
    ggplot(aes(x = rank, y = height)) +
    scale_colour_identity() +
    new_scale_color() +
    geom_chicklet(aes(fill = rank, colour = rank)) +
    geom_image(aes(y = height + 0.5, image = image), size = 0.25, by = "width") +
    geom_point(aes(x = rank, y = height / 2), size = 15, colour = "white") +
    geom_text(aes(x = rank, y = height / 2, label = rank, colour = rank), size = 8, fontface = "bold", family = font) +
    scale_fill_manual(values = c("#CACF85", "#8CBA80", "#658E9C")) +
    scale_colour_manual(values = c("#CACF85", "#8CBA80", "#658E9C")) +
    scale_y_continuous(limits = c(0, 3)) +
    ggtitle(unique(data[["year"]])) +
    theme_minimal(16, base_family = font) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0.5),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    )
}

year_plots <- split(top_3_names, top_3_names$year) %>%
  map(plot_ranked_names)

dog_legend <- top_3_names %>%
  distinct(name, image, colour) %>%
  mutate(name = fct_relevel(name, "Max", "Charlie", "Buddy", "Bella", "Luna")) %>%
  arrange(name) %>%
  mutate(y = row_number()) %>%
  ggplot(aes(y = -y)) +
  geom_text(aes(x = 1, label = name, colour = colour), size = 8, hjust = 0, family = font) +
  geom_image(aes(x = 1, image = image), size = 0.15, by = "width", hjust = 1) +
  scale_colour_identity() +
  scale_x_continuous(limits = c(-0.5, 4)) +
  scale_y_continuous(limits = c(-6, 0)) +
  theme_void()

p <- wrap_plots(c(
  list(dog_legend),
  year_plots
), nrow = 2) +
  plot_annotation(
    title = "Most popular dog names in Toronto",
    subtitle = str_wrap("<b style='color:#ED6A5A;'>Charlie</b> has consistently been the most popular name since 2012. <b style='color:#DA3E52'>Max</b> held strong in second place until 2016, when it was<br>overtaken by <b style='color:#2C3D55'>Bella</b>. <b style='color:#4C061D'>Luna</b> suddenly took over second place in 2020, despite never appearing in the top 3 before!", width = 100),
    caption = "Data: Open Data Toronto | Viz: Sharla Gelfand | Icons: svgrepo.com/collection/woof-woof",
    theme = theme_minimal(16, base_family = font) + theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 30),
      plot.subtitle = element_markdown()
    )
  )

ggsave(here::here("08-animals", "animals.png"), width = 12, height = 7, dpi = 300)

top_3_names <- top_3_names %>%
  select(year, rank, name) %>%
  mutate(rank = as.character(rank),
         rank = as.numeric(rank)) %>%
  arrange(year, rank)

write_csv(top_3_names, here::here("08-animals", "top_3_names_by_year.csv"))

# Vectors: https://www.svgrepo.com/collection/woof-woof/
