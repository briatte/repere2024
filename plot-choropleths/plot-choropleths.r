# maps of electorate populations
# labels show 9 largest cities (> 40,000 hab.)

library(ggrepel)
library(sf)
library(tidyverse)

geo <- readr::read_rds("data-cities/geography/city-contours.rds")
ele <- read_tsv("data-cities/electorate/electorate-counts.tsv",
                col_types = cols()) %>%
  mutate(code = as.character(code)) %>%
  # replace 0 with NA to grey out areas with 0 voters
  mutate_if(is.numeric, na_if, 0)

geo <- full_join(geo, ele, by = "code")

ctr <- readr::read_rds("data-cities/geography/city-centres.rds")
pop <- read_tsv("data-cities/population/population.tsv", col_types = "cci")

ctr_largest <- full_join(ctr, pop, by = "code") %>%
  filter(pop21 > 4e4)

lbl_largest <- ggrepel::geom_label_repel(
  data = ctr_largest,
  aes(label = city, geometry = geometry),
  stat = "sf_coordinates",
  box.padding = 0.7, max.overlaps = Inf,
  min.segment.length = 0, seed = 202459
)

map_crs <- coord_sf(crs = "EPSG:9850") # RGF93 v2b / CC50

theme_map <- theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "inside", legend.position.inside = c(0.2, 0.2))

# Electorate --------------------------------------------------------------

lbl <- format(10^(2:5), big.mark = ",", scientific = FALSE, trim = TRUE)

ggplot(geo) +
  geom_sf(aes(fill = log10(n_ins))) +
  lbl_largest +
  geom_sf(data = ctr_largest) +
  map_crs +
  scale_fill_viridis_b("", option = "C", breaks = 2:5, labels = lbl) +
  theme_map +
  labs(title = "Size of total electorate",
       subtitle = "All nationalities")

ggsave("plot-choropleths/choropleth-n_ins.jpg", width = 8, height = 8)

# Non-French --------------------------------------------------------------

lbl <- format(10^(1:3), big.mark = ",", scientific = FALSE, trim = TRUE)

ggplot(geo) +
  geom_sf(aes(fill = log10(n_eur))) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  scale_fill_viridis_b("", option = "C", breaks = 1:3, labels = lbl) +
  theme_map +
  labs(title = "Size of non-French electorate",
       subtitle = "All nationalities",
       caption = "Areas in grey are cities with 0 non-French voters")

ggsave("plot-choropleths/choropleth-n_eur.jpg", width = 8, height = 8)

# as % of voters

pct <- c(1.25, 2.5, 5, 10, 20, 40)

ggplot(geo) +
  # grey out 0% areas
  geom_sf(aes(fill = if_else(n_eur == 0, NA, 100 * n_eur / n_ins))) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  scale_fill_viridis_b("", option = "C", breaks = pct) +
  theme_map +
  labs(title = "% of non-French voters out of total electorate",
       subtitle = "All nationalities",
       caption = "Areas in grey are cities with 0 non-French voters")

ggsave("plot-choropleths/choropleth-pct_eur.jpg", width = 8, height = 8)

# Nationalities -----------------------------------------------------------

nat_breaks <- c(5, 10, 20, 40, 80, 160) %>%
  scale_fill_viridis_b("", option = "C", breaks = .)

# BE

ggplot(geo) +
  geom_sf(aes(fill = n_be)) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  nat_breaks +
  theme_map +
  labs(title = "Size of Belgian electorate",
       caption = "Areas in grey are cities with 0 Belgian voters")

ggsave("plot-choropleths/choropleth-n_be.jpg", width = 8, height = 8)

# as % of voters -- uninteresting, never goes above 0.5%
geo %>%
  mutate(pct_be = n_be / n_ins) %>%
  pull(pct_be) %>%
  summary()

# PT

ggplot(geo) +
  geom_sf(aes(fill = n_pt)) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  nat_breaks +
  theme_map +
  labs(title = "Size of Portuguese electorate",
       caption = "Areas in grey are cities with 0 Portuguese voters")

ggsave("plot-choropleths/choropleth-n_pt.jpg", width = 8, height = 8)

# IT

ggplot(geo) +
  geom_sf(aes(fill = n_it)) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  nat_breaks +
  theme_map +
  labs(title = "Size of Italian electorate",
       caption = "Areas in grey are cities with 0 Italian voters")

ggsave("plot-choropleths/choropleth-n_it.jpg", width = 8, height = 8)

# ES

ggplot(geo) +
  geom_sf(aes(fill = n_es)) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  nat_breaks +
  theme_map +
  labs(title = "Size of Spanish electorate",
       caption = "Areas in grey are cities with 0 Spanish voters")

ggsave("plot-choropleths/choropleth-n_es.jpg", width = 8, height = 8)

# DE

ggplot(geo) +
  geom_sf(aes(fill = n_de)) +
  geom_sf(data = ctr_largest) +
  lbl_largest +
  map_crs +
  nat_breaks +
  theme_map +
  labs(title = "Size of German electorate",
       caption = "Areas in grey are cities with 0 German voters")

ggsave("plot-choropleths/choropleth-n_de.jpg", width = 8, height = 8)

# kthxye
