library(raybonsai)
library(rayrender)
library(rayshader)
library(readr)
library(dplyr)
library(purrr)
library(scico)

forest_area <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv")

birth_year_forest_area <- forest_area %>%
  filter(year == 1991) %>%
  select(entity, forest_area)

colors <- scico(nrow(birth_year_forest_area), palette = "bamako")

generate_tree_by_forest_area <- function(forest_area, seed) {
  generate_tree(
    seed = seed,
    branch_depth = case_when(
      forest_area > 10 ~ round(forest_area / 10),
      forest_area > 1 ~ round(forest_area),
      forest_area > 0.1 ~ round(10 * forest_area),
      TRUE ~ round(forest_area * 100)
    ),
    leaf_depth_start = 3,
    leaf_color = sample(colors, 1),
    branch_color = sample(colors, 1),
    branch_angle = runif(10, min = -30, max = 30)
  )
}

trees_df <- birth_year_forest_area %>%
  mutate(
    id = row_number(),
    tree = map2(forest_area, id, generate_tree_by_forest_area)
  )

trees <- trees_df[["tree"]]

first_tree <- trees[[1]]
remaining_trees <- trees[-1]

scene <- group_objects(
  first_tree,
  pivot_point = c(0, -10, 0), group_angle = c(0, 0, 10)
)

build_scene <- function(tree, angle_z) {
  scene <<- scene %>%
    add_object(
      group_objects(tree, pivot_point = c(0, -10, 0), group_angle = c(runif(1, -90, 90), runif(1, -90, 90), angle_z))
    )
}

walk2(remaining_trees, runif(length(remaining_trees), -90, 90), build_scene)

# Background source: https://hdrihaven.com/hdri/?c=outdoor&h=kloppenheim_02
scene %>%
  render_tree(
    lights = FALSE, environment_light = here::here("07-physical", "kloppenheim_02_2k.hdr"),
    samples = 40,
    fov = 20, lookat = c(2, 4, 2), lookfrom = c(40, 4, 10),
    aperture = 0.5,
    width = 1200, height = 800,
    ground_color1 = sample(colors, 1), ground_color2 = sample(colors, 1)
  )
