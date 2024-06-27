# plot fieldwork completeness
# master data is exported from Google Drive

library(cartogram)
library(sf)
library(tidyverse)

d <- "data-lists/coverage/coverage.tsv" %>%
  read_tsv(col_types = "cciiii") %>%
  mutate(status = case_when(observed == 1 & empty == 1 ~ "Observed but empty",
                            observed == 0 ~ "Unobserved, missing",
                            .default = "Observed") %>%
           factor())

table(d$status)

map_crs <- coord_sf(crs = "EPSG:9850") # RGF93 v2b / CC50

theme_map <- theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "inside", legend.position.inside = c(0.2, 0.2))

# Cartogram ---------------------------------------------------------------

lbl_largest <- function(data) {
  ggrepel::geom_label_repel(
    data = data,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 0.7, max.overlaps = Inf,
    min.segment.length = 0, seed = 202459
  )
}

f <- "plot-cartograms/contiguous-cartogram-eur24.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(eur24, "ins", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

geo_ccar_eur24 <- readr::read_rds(f) %>%
  full_join(select(d, -city), by = "code")

geo_ccar_eur24_ctr <- sf::st_centroid(filter(geo_ccar_eur24, ins >= 2e4))

ggplot(geo_ccar_eur24) +
  geom_sf(aes(fill = status)) +
  geom_sf(data = geo_ccar_eur24_ctr) +
  lbl_largest(geo_ccar_eur24_ctr) +
  map_crs +
  scale_fill_brewer("", palette = "Set2") +
  theme_map +
  labs(title = "Data coverage",
       subtitle = "Source: fieldwork notes, 17-21 June 2024",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("data-lists/coverage/coverage-cartogram.jpg", width = 8, height = 8)

# Choropleth --------------------------------------------------------------

geo <- readr::read_rds("data-cities/geography/city-contours.rds") %>%
  full_join(select(d, -city), by = "code")

ctr <- readr::read_rds("data-cities/geography/city-centres.rds")
pop <- read_tsv("data-cities/population/population.tsv", col_types = "cci")

ctr_largest <- full_join(ctr, pop, by = "code") %>%
  filter(pop21 > 4e4)

ggplot(geo) +
  geom_sf(aes(fill = status)) +
  geom_sf(data = ctr_largest) +
  lbl_largest(ctr_largest) +
  map_crs +
  scale_fill_brewer("", palette = "Set2") +
  theme_map +
  labs(title = "Data coverage",
       subtitle = "Source: fieldwork notes, 17-21 June 2024",
       caption = "Labels show cities with 40,000+ residents")

ggsave("data-lists/coverage/coverage-choropleth.jpg", width = 8, height = 8)

# kthxbye
