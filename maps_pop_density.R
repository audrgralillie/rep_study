# MAP 1: Basic overview
# I want uniform colors for each landscape category
landscape_colors <- c(
  "Exurban" = "#c5e8b7",
  "Suburban Low" = "#abe098",
  "Suburban High" = "#83d475",
  "Urban Low" = "#57c84d",
  "Urban High" = "#2eb62c")

# basic plot depicting landscapes
library(ggplot2)
ggplot(denver_pop_tract) +
  geom_sf(aes(fill = landscape), color = NA) +
  scale_fill_manual(values = landscape_colors) +
  labs(
    title = "Landscape Classification of Denver MSA",
    fill = "Landscape Type"
  ) +
  theme_minimal()

# add median income?
library(tidycensus)

denver_med_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "CO",
  county = denver_counties,
  year = 2020)

# join median income to population
denver_pop_tract <- denver_pop_tract %>%
  left_join(denver_med_income, by = "GEOID")

# graph
ggplot(denver_pop_tract, aes(x = landscape, y = estimate, fill = landscape)) +
  geom_boxplot() +
  scale_fill_manual(values = landscape_colors) +
  labs(
    title = "Median Household Income by Landscape (Population Density) Type",
    x = "Landscape Type",
    y = "Median Income"
  ) +
  theme_minimal()


# MAP 2
ggplot(denver_pop_tract) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  labs(
    title = "Median Household Income in Denver MSA",
    fill = "Income"
  ) +
  theme_minimal()


ggsave("basicmap.png", width = 16, height = 8)
ggsave("incomemap.png", width = 16, height = 8)
ggsave("incomeboxplot", width = 16, height = 8)