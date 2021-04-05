library(cranlogs)
library(ggplot2)
library(dplyr)

downloads <- cran_downloads(package = "opendatatoronto", from = "2019-10-28", to = "2021-04-05")

downloads_cumulative <- downloads %>%
  arrange(date) %>%
  mutate(count = cumsum(count))

ggplot() +
  geom_line(data = downloads_cumulative, aes(x = date, y = count), size = 1, colour = "#D58936") +
  theme_void()

ggsave(here::here("05-slope", "slope-bare.png"), bg = "transparent")
