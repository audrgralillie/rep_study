# MAP 1: Basic overview
library(ggplot2)
ggplot(denver_pop_tract) +
  geom_sf(aes(fill = landscape), color = NA) +
  scale_fill_brewer(palette = "Set4") +
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
ggplot(denver_pop_tract, aes(x = landscape, y = estimate)) +
  geom_boxplot() +
  labs(
    title = "Median Household Income by Landscape",
    x = "Landscape Type",
    y = "Median Income"
  ) +
  theme_minimal()
