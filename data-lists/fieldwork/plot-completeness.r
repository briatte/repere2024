# plot fieldwork completeness
# master data is exported from Google Drive

library(cartogram)
library(sf)
library(tidyverse)

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

geo_ccar_eur24 <- readr::read_rds(f)
geo_ccar_eur24_ctr <- sf::st_centroid(filter(geo_ccar_eur24, ins >= 2e4))

d <- "data-lists/fieldwork/LISTE-COMMUNES-NORD-UGLE - EXPORT.tsv" %>%
  read_tsv(col_types = "cccccii") %>%
  mutate(complete = case_when(
    str_detect(missing, "^tous") ~ "Observed but empty",
    str_detect(missing, "manquante|non traitée") ~ "Unobserved, missing",
               .default = "Observed"))

table(d$complete)

geo_ccar_eur24 <- full_join(geo_ccar_eur24, select(d, -city), by = "code")

ggplot(geo_ccar_eur24) +
  geom_sf(aes(fill = factor(complete))) +
  geom_sf(data = geo_ccar_eur24_ctr) +
  lbl_largest(geo_ccar_eur24_ctr) +
  map_crs +
  scale_fill_brewer("", palette = "Set2") +
  theme_map +
  labs(title = "Data completeness",
       subtitle = "Source: fieldwork notes, 17-21 June 2024",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("data-lists/fieldwork/completeness-cartogram.jpg", width = 8, height = 8)

# Choropleth --------------------------------------------------------------

geo <- readr::read_rds("data-cities/geography/city-contours.rds") %>%
  full_join(select(d, -city), by = "code")

ctr <- readr::read_rds("data-cities/geography/city-centres.rds")
pop <- read_tsv("data-cities/population/population.tsv", col_types = "cci")

ctr_largest <- full_join(ctr, pop, by = "code") %>%
  filter(pop21 > 4e4)

ggplot(geo) +
  geom_sf(aes(fill = factor(complete))) +
  geom_sf(data = ctr_largest) +
  lbl_largest(ctr_largest) +
  map_crs +
  scale_fill_brewer("", palette = "Set2") +
  theme_map +
  labs(title = "Data completeness",
       subtitle = "Source: fieldwork notes, 17-21 June 2024",
       caption = "Labels show cities with 40,000+ residents")

ggsave("data-lists/fieldwork/completeness-choropleth.jpg", width = 8, height = 8)

# Relationship to electorate size -----------------------------------------

table(d$complete)
round(100 * prop.table(table(d$complete)), 1)

# covered cities
sum(d$complete != "Unobserved, missing") # 580
round(100 * sum(d$complete != "Unobserved, missing") / nrow(d), 1) # 89.5%

d <- d %>%
  full_join("data-cities/electorate/electorate-counts.tsv" %>%
              read_tsv(col_types = cols()) %>%
              mutate(code = as.character(code), n_ins1000 = n_ins / 1000) %>%
              select(-city),
            by = "code")

# covered voters
sum(d$n_ins[ d$complete != "Unobserved, missing" ]) # 1,757,771
round(100 * sum(d$n_ins[ d$complete != "Unobserved, missing" ]) /
        sum(d$n_ins), 1) # 95.7%

median(d$n_ins[ d$complete == "Observed" ]) # 1,419
median(d$n_ins[ d$complete != "Observed" ]) # 618

median(d$n_ins[ d$complete != "Unobserved, missing" ]) # 1,018
median(d$n_ins[ d$complete == "Unobserved, missing" ]) # 785

median(d$n_ins[ d$complete == "Observed but empty" ]) # 538

xy <- bind_cols(d, as_tibble(st_coordinates(ctr))) %>%
  mutate(upper = Y > median(xy$Y))

glm(complete == "Unobserved, missing" ~ upper + n_ins, data = xy) %>%
  coef()

# kthxbye
