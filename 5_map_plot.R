library(ggplot2)
library(mapproj)

mrp.prediction <- readRDS("Data/mrp_pred.rds")

mrp.prediction.map <- map_data("state")
mrp.prediction.map$opinion <-
  mrp.prediction$opinion[match(mrp.prediction.map$region, tolower(mrp.prediction$state.abb))]

ggplot(mrp.prediction.map, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = opinion)) +
  scale_fill_distiller(palette = "Spectral", labels = scales::percent) +
  coord_map('mercator') +
  labs(fill = "Percent favoring Deporting Dreamers") +
  theme_void() +
  theme(legend.position="bottom", legend.box="horizontal",
        legend.key.width=grid::unit(.1,'npc')) +
  guides(fill = guide_colourbar(title.position="top",
                                title.hjust = 0.5))
