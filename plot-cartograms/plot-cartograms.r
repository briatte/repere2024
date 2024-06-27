# plot cartograms of abstention and turnout in recent elections
# areas are sized according to their total electorate

library(cartogram)
library(sf)
library(tidyverse)

geo <- "data-cities/geography/city-contours.rds" %>%
  readr::read_rds() %>%
  st_transform("EPSG:2154")

lbl_largest <- function(data) {
  ggrepel::geom_label_repel(
    data = data,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 0.7, max.overlaps = Inf,
    min.segment.length = 0, seed = 202459
  )
}

map_crs <- coord_sf(crs = "EPSG:9850") # RGF93 v2b / CC50

theme_map <- theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "inside", legend.position.inside = c(0.2, 0.2))

# Electoral data ----------------------------------------------------------

# Eur. 2024

eur24 <- "data-elections/eur-2024/turnout-eur-2024.tsv" %>%
  readr::read_tsv(col_types = "ccii") %>%
  mutate(p_abs = 100 * abs / ins, p_trn = 100 - p_abs) %>%
  full_join(geo, ., by = "code")

# Pres. 2022 Round 1

pre22 <- "data-elections/pre-2022/turnout-pre-2022-r1.tsv" %>%
  readr::read_tsv(col_types = "ccii") %>%
  mutate(p_abs = 100 * abs / ins, p_trn = 100 - p_abs) %>%
  full_join(geo, ., by = "code")

# Mun. 2020 Round 1

mun20 <- "data-elections/mun-2020/turnout-mun-2020-r1.tsv" %>%
  readr::read_tsv(col_types = "ccii") %>%
  mutate(p_abs = 100 * abs / ins, p_trn = 100 - p_abs) %>%
  full_join(geo, ., by = "code")

# Continuous cartograms ---------------------------------------------------

# Eur. 2024

f <- "plot-cartograms/contiguous-cartogram-eur24.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(eur24, "ins", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

geo_ccar_eur24 <- readr::read_rds(f)
geo_ccar_eur24_ctr <- sf::st_centroid(filter(geo_ccar_eur24, ins >= 2e4))

ggplot(geo_ccar_eur24) +
  geom_sf(aes(fill = p_abs)) +
  geom_sf(data = geo_ccar_eur24_ctr) +
  lbl_largest(geo_ccar_eur24_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in EU elections, 2024",
       subtitle = "All nationalities",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_abs-eur24.jpg", width = 8, height = 8)

ggplot(geo_ccar_eur24) +
  geom_sf(aes(fill = p_trn)) +
  geom_sf(data = geo_ccar_eur24_ctr) +
  lbl_largest(geo_ccar_eur24_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in EU elections, 2024",
       subtitle = "All nationalities",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_trn-eur24.jpg", width = 8, height = 8)

# Pres. 2022 Round 1

f <- "plot-cartograms/contiguous-cartogram-pre22.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(pre22, "ins", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

geo_ccar_pre22 <- readr::read_rds(f)
geo_ccar_pre22_ctr <- sf::st_centroid(filter(geo_ccar_pre22, ins >= 2e4))

ggplot(geo_ccar_pre22) +
  geom_sf(aes(fill = p_abs)) +
  geom_sf(data = geo_ccar_pre22_ctr) +
  lbl_largest(geo_ccar_pre22_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in presidential election, 2022",
       subtitle = "All nationalities, first round",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_abs-pre22.jpg", width = 8, height = 8)

ggplot(geo_ccar_pre22) +
  geom_sf(aes(fill = p_trn)) +
  geom_sf(data = geo_ccar_pre22_ctr) +
  lbl_largest(geo_ccar_pre22_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in presidential election, 2022",
       subtitle = "All nationalities, first round",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_trn-pre22.jpg", width = 8, height = 8)

# Mun. 2020 Round 1

f <- "plot-cartograms/contiguous-cartogram-mun20.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(mun20, "ins", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

geo_ccar_mun20 <- readr::read_rds(f)
geo_ccar_mun20_ctr <- sf::st_centroid(filter(geo_ccar_mun20, ins >= 2e4))

ggplot(geo_ccar_mun20) +
  geom_sf(aes(fill = p_abs)) +
  geom_sf(data = geo_ccar_mun20_ctr) +
  lbl_largest(geo_ccar_mun20_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in municipal elections, 2020",
       subtitle = "All nationalities, first round",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_abs-mun20.jpg", width = 8, height = 8)

ggplot(geo_ccar_mun20) +
  geom_sf(aes(fill = p_abs)) +
  geom_sf(data = geo_ccar_mun20_ctr) +
  lbl_largest(geo_ccar_mun20_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in municipal elections, 2020",
       subtitle = "All nationalities, first round",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/cartogram-pct_trn-mun20.jpg", width = 8, height = 8)

# Dorling cartograms (non-overlapping circles) ----------------------------

# Eur. 2024

f <- "plot-cartograms/dorling-cartogram-eur24.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_dorling(eur24, "ins", k = 0.6) %>%
    readr::write_rds(f)

}

geo_nocc_eur24 <- readr::read_rds(f)
geo_nocc_eur24_ctr <- sf::st_centroid(filter(geo_nocc_eur24, ins >= 2e4))

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_eur24, aes(fill = p_abs), color = "grey25") +
  geom_sf(data = geo_nocc_eur24_ctr) +
  lbl_largest(geo_nocc_eur24_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in EU elections, 2024",
       subtitle = "All nationalities",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_abs-eur24.jpg", width = 8, height = 8)

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_eur24, aes(fill = p_trn), color = "grey25") +
  geom_sf(data = geo_nocc_eur24_ctr) +
  lbl_largest(geo_nocc_eur24_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in EU elections, 2024",
       subtitle = "All nationalities",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_trn-eur24.jpg", width = 8, height = 8)

# Pres. 2022 Round 1

f <- "plot-cartograms/dorling-cartogram-pre22.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_dorling(pre22, "ins", k = 0.6) %>%
    readr::write_rds(f)

}

geo_nocc_pre22 <- readr::read_rds(f)
geo_nocc_pre22_ctr <- sf::st_centroid(filter(geo_nocc_pre22, ins >= 2e4))

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_pre22, aes(fill = p_abs), color = "grey25") +
  geom_sf(data = geo_nocc_pre22_ctr) +
  lbl_largest(geo_nocc_pre22_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in presidential election, 2022",
       subtitle = "All nationalities, first round",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_abs-pre22.jpg", width = 8, height = 8)

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_pre22, aes(fill = p_trn), color = "grey25") +
  geom_sf(data = geo_nocc_pre22_ctr) +
  lbl_largest(geo_nocc_pre22_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in presidential election, 2022",
       subtitle = "All nationalities, first round",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_trn-pre22.jpg", width = 8, height = 8)

# Mun. 2020 Round 1

f <- "plot-cartograms/dorling-cartogram-mun20.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_dorling(mun20, "ins", k = 0.6) %>%
    readr::write_rds(f)

}

geo_nocc_mun20 <- readr::read_rds(f)
geo_nocc_mun20_ctr <- sf::st_centroid(filter(geo_nocc_mun20, ins >= 2e4))

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_mun20, aes(fill = p_abs), color = "grey25") +
  geom_sf(data = geo_nocc_mun20_ctr) +
  lbl_largest(geo_nocc_mun20_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Abstention rate in municipal elections, 2020",
       subtitle = "All nationalities, first round",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_abs-mun20.jpg", width = 8, height = 8)

ggplot(geo) +
  geom_sf(fill = "grey90", color = "white") +
  geom_sf(data = geo_nocc_mun20, aes(fill = p_trn), color = "grey25") +
  geom_sf(data = geo_nocc_mun20_ctr) +
  lbl_largest(geo_nocc_mun20_ctr) +
  scale_fill_viridis_b("", option = "C") +
  scale_colour_viridis_b("", option = "C") +
  theme_map +
  labs(title = "Turnout rate in municipal elections, 2020",
       subtitle = "All nationalities, first round",
       caption = str_c("Circle areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("plot-cartograms/dorling-pct_trn-mun20.jpg", width = 8, height = 8)

# kthxbye
