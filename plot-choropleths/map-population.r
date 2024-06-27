# map of legal populations
# labels show 9 largest cities (> 40,000 hab.)

library(ggrepel)
library(sf)
library(tidyverse)

geo <- readr::read_rds("geography/city-contours.rds")
ctr <- readr::read_rds("geography/city-centres.rds")

pop <- read_tsv("population/population.tsv", col_types = "cci")

ctr_largest <- full_join(ctr, pop, by = "code") %>%
  filter(pop21 > 4e4)

lbl <- format(10^(2:5), big.mark = ",", scientific = FALSE, trim = TRUE)

ggplot(full_join(geo, pop, by = "code")) +
  geom_sf(aes(fill = log10(pop21))) +
  geom_sf(data = ctr_largest) +
  ggrepel::geom_label_repel(
    data = ctr_largest,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 1, max.overlaps = Inf,
    min.segment.length = 0
  ) +
  coord_sf(crs = "EPSG:9850") + # RGF93 v2b / CC50
  scale_fill_viridis_b("Population", option = "C", values = 2:5, labels = lbl) +
  theme_void()

ggsave("map-population.jpg", width = 9, height = 9)

# kthxye
