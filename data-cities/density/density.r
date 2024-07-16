# population density, computed and as categorized by Insee
# categories explained: https://www.insee.fr/fr/information/2114627

library(cartogram)
library(sf)
library(tidyverse)

d <- readr::read_tsv("data-cities/population/population.tsv",
                     show_col_types = FALSE) %>%
  mutate(code = as.character(code))

# Insee density categories ------------------------------------------------

u <- str_c("https://www.insee.fr/fr/statistiques/fichier/2114627/",
           "grille_densite_niveau_detaille_2022.xlsx")

f <- str_c("data-cities/density/", fs::path_file(u))

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = TRUE)
}

d <- readxl::read_excel(f, sheet = 1) %>%
  rename_with(~ str_remove_all(.x, "\\\n")) %>%
  select(code = `Code Commune`,
         city = `Libellé des communes`,
         density4 = `Typo degré de Densité`,
         pop18 = `Population municipale 2018`,
         dens_p1 = `Part population Dense (1)`,
         dens_p2 = `Part population Intermédiaire (2)`,
         dens_p3 = `Part population Peu dense (3)`,
         dens_p4 = `Part population Très peu dense (4)`) %>%
  filter(str_detect(code, "^59")) %>%
  mutate(pop18 = as.integer(pop18)) %>%
  full_join(select(d, -city), by = "code")

stopifnot(nrow(d) == 648)

# manual estimates --------------------------------------------------------

geo <- readr::read_rds("data-cities/geography/city-contours.rds") %>%
  st_transform("EPSG:2154") %>%
  full_join(d, by = "code") %>%
  # population density
  mutate(area = units::set_units(sf::st_area(geo), km^2),
         density = as.numeric(pop21 / area),
         log_density = log10(density))

# top cities by density
arrange(select(geo, city, area, density, log_density), -density)

# Lille should be close to 6,859 hab./km2 (source: fr.wikipedia.org)
stopifnot(near(geo$density[ geo$city == "Lille" ], 6859, tol = 50))

# sanity checks: no missing values
stopifnot(!is.na(geo$density))
stopifnot(!is.na(geo$density4))

# compare 2018 and 2021 populations
with(d, cor(pop18, pop21))  # 0.99
with(d, max(pop21 - pop18)) # 4067

# a few remarkable cases (threshold: +1,000 or +10%)
filter(d, (pop21 - pop18) > 1000 | (pop21 - pop18) > (0.1 * pop21)) %>%
  mutate(delta = pop21 - pop18) %>%
  arrange(-delta)

# maps --------------------------------------------------------------------

# use what we computed in `census`
f <- "data-cities/census/contiguous-cartogram-pop21.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(geo, "pop21", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

lbl <- c("Very dense", "Dense", "Sparse", "Very sparse")

geo_ccar_pop <- readr::read_rds(f) %>%
  full_join(select(as_tibble(geo), -city, -pop21, -geometry), by = "code") %>%
  mutate(density4 = factor(density4, labels = lbl))

geo_ccar_pop_ctr <- sf::st_centroid(filter(geo_ccar_pop, pop21 >= 4e4))

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

brk <- log10(c(20, 200, 2e3))
lbl <- format(10^brk, big.mark = ",", scientific = FALSE, trim = TRUE)

# continuous
p1 <- ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = log_density)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C", breaks = brk, labels = lbl) +
  theme_map +
  labs(title = "Population density",
       subtitle = "Residents / km\U00B2 (2021)")

# categorical
p2 <- ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = density4)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_d("", option = "C", direction = -1) +
  theme_map +
  labs(title = "Population density",
       subtitle = "Insee categorization (2022)")

p1 + p2 + plot_annotation(
  caption = str_c("Areas are proportional to population size\n",
                  "Labels show cities with 40,000+ residents")
)

ggsave("data-cities/density/density.jpg", width = 16, height = 8)

# export ------------------------------------------------------------------

select(as_tibble(geo), code, density, density4, -geometry) %>%
  mutate_if(is.numeric, round, 2) %>%
  readr::write_tsv("data-cities/density/density.tsv")

# kthxbye
