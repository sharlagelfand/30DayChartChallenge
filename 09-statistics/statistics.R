library(ggplot2)
library(dplyr)
library(forcats)
library(ggforce)
library(showtext)
library(ggnewscale)

font <- "Gloria Hallelujah"
font_add_google(font)
showtext_auto()

set.seed(1234)

normal <- tibble(x = rnorm(1000),
                 distribution = "normal")
bimodal <- tibble(x = c(rnorm(300, sd = 0.5), rnorm(1000, mean = 3)),
                  distribution = "bimodal")

distributions <- normal %>%
  bind_rows(bimodal) %>%
  mutate(distribution = fct_relevel(distribution, "normal", "bimodal"))

x_start <- -3.75
x_end <- 6.5

p <- ggplot() +
  geom_density(data = distributions %>% filter(distribution == "normal"), aes(x = x - 0.1, fill = distribution), size = 2, colour = NA) +
  geom_density(data = distributions %>% filter(distribution == "normal"), aes(x = x, colour = distribution), size = 2, fill = NA) +
  geom_density(data = distributions %>% filter(distribution != "normal"), aes(x = x + 0.1, fill = distribution), size = 2, colour = NA) +
  geom_density(data = distributions %>% filter(distribution != "normal"), aes(x = x, colour = distribution), size = 2, fill = NA) + scale_fill_manual(values = rev(c("#F0A43C", "#66CBFA"))) +
  scale_colour_manual(values = rev(c("#EC7C33", "#3D7EBB"))) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

p <- p +
  geom_text(aes(x = 3, y = 0.4, label = "you're not normal."), colour = "#EC7C33", fontface = "bold", angle = 10, family = font, size = 8) +
  geom_ellipse(aes(x0 = 3, y0 = 0.4, a = 2.25, b = 0.05, angle = 0.01), size = 2, colour = "#797A7B") +
  geom_segment(aes(x = 0.9, xend = 1.3, y = 0.3, yend = 0.35), size = 2, colour = "#797A7B", lineend = "round")

p <- p +
  geom_segment(aes(x = x_start, xend = 6.5, y = 0, yend = 0), size = 2, colour = "#797A7B", lineend = "round") +
  geom_segment(aes(x = x_start, xend = x_start, y = 0, yend = 0.5), size = 2, colour = "#797A7B", lineend = "round")

eyes <- tribble(
  ~x, ~y, ~size, ~distribution,
  -0.15, 0.3, 2, "normal",
  0.3, 0.305, 2.5, "normal",
  2.7, 0.21, 3.5, "bimodal",
  3.1, 0.2075, 6, "bimodal"
)

mouths <- tribble(
  ~x, ~xend, ~y, ~yend, ~size, ~curvature, ~distribution,
  0, 0.2, 0.275, 0.2775, 1, 0, "normal",
  2.75, 3.1, 0.195, 0.185, 1.5, 0.5, "bimodal"
)

p <- p +
  new_scale_colour() +
  geom_point(data = eyes, aes(x = x, y = y, size = size, colour = distribution)) +
  geom_curve(data = mouths %>%
               filter(distribution == "bimodal"), aes(x = x, xend = xend, y = y, yend = yend, size = size, colour = distribution), lineend = "round") +
  geom_segment(data = mouths %>%
                 filter(distribution == "normal"), aes(x = x, xend = xend, y = y, yend = yend, size = size, colour = distribution), lineend = "round") +
  scale_colour_manual(values = rev(c("#5C1712", "#132C43"))) +
  scale_size_identity()

p <- p +
  theme_void(base_family = font) +
  theme(legend.position = "none",
        plot.caption = element_text())

p <- p +
  labs(caption = "Visualization: Sharla Gelfand | Original art by Allison Horst") +
  theme(plot.caption = element_text(colour = "#132C43"))

ggsave(here::here("09-statistics/statistics.png"), p, dpi = 500, width = 9, height = 5)

