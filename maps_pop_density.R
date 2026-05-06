# MAP 1: Basic overview
library(ggplot2)

ggplot(denver_pop_tract) +
  geom_sf(aes(fill = landscape), color = NA) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Landscape Classification of Denver MSA",
    fill = "Landscape Type"
  ) +
  theme_minimal()