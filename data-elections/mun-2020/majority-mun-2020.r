# extract municipal majorities from zipped results, and plot map
# expects the contiguous cartogram to have been pre-computed

library(rvest)
library(sf)
library(tidyverse)

# parse -------------------------------------------------------------------

z <- "data-elections/mun-2020/html.zip"
j <- str_subset(unzip(z, list = TRUE)$Name, "/cities/.*html")

d <- tibble()
for (i in j) {

  h <- read_html(unz(z, i))
  v <- html_nodes(h, "table.fr-table") %>%
    map(html_table)

  w <- which(map_chr(v, ~ first(names(.x))) == "Liste conduite par")

  if (!length(w))
    next

  w <- first(which(map_chr(v, ~ names(.x)[2]) == "Voix"))
  d <- bind_rows(d, add_column(v[[ w ]][, c(1, 4, 5)], code = i, .before = 1))

}

# aggregate ---------------------------------------------------------------

d <- transmute(d, code = str_extract(code, "\\d{5}"),
               list = str_extract(`Liste conduite par`, "\\(.*?\\)") %>%
                 str_remove_all("\\(|\\)") %>%
                 str_remove("^L"),
               # forget about shares (council control is what we are after)
               # share = as.double(str_replace(`% exprimés`, ",", ".")),
               seats = `Sièges au conseil municipal`) %>%
  # drop unknowns
  filter(!is.na(list)) %>%
  # aggregate all DVDs, DVGs etc. together at the city-level
  group_by(code, list) %>%
  summarise(seats = sum(seats)) %>%
  # find majority list(s)
  group_by(code) %>%
  summarise(maj = list[ which.max(seats) ]) %>%
  # add missing cities
  full_join(tibble(code = str_extract(j, "\\d{5}")), by = "code") %>%
  mutate(
    maj5 = case_when(
      # 2020: LEXG, LFG
      maj %in% c("COM") ~ "Far-left",
      # 2020: DVG, UG, VEC, FI, SOC, RDG, COM (debatable), ECO
      maj %in% c("SOC", "UG", "DVG", "VEC") ~ "Left",
      # 2020: UDI, REM, DVC, UC, MDM
      maj %in% c("DVC", "UC", "UDI") ~ "Centre",
      # 2020: LR, DVD, DLF (debatable), UD, UMP
      maj %in% c("DVD", "LR", "UD") ~ "Right",
      # N.B. 2024: no far-right majority in the Nord for known cities
      maj == "DIV" ~ "Other",
      .default = "Other"
    ),
    # aggregate far-left (n = 8) into left
    maj4 = if_else(maj5 == "Far-left", "Left", maj5)) %>%
  arrange(code)

stopifnot(!is.na(d$maj4))
stopifnot(!is.na(d$maj5))

sum(is.na(d$maj)) # n = 499 unknowns, coded as 'other'
count(d, maj4)    # n = 61 left, 21 centre, 56 right
count(d, maj5)

# export
select(d, code, maj, maj4) %>%
  readr::write_tsv("data-elections/mun-2020/majority-mun-2020.tsv")

# map ---------------------------------------------------------------------

# use what we computed in `plot-cartograms`
f <- "plot-cartograms/contiguous-cartogram-mun20.rds"
if (!fs::file_exists(f)) {
  stop("run 'plot-cartograms/plot-cartograms.r' to generate cartogram")
}

geo_ccar_mun20 <- readr::read_rds(f) %>%
  full_join(d, by = "code") %>%
  mutate(maj4 = factor(maj4, c("Left", "Centre", "Right", "Other")))

geo_ccar_mun20_ctr <- sf::st_centroid(filter(geo_ccar_mun20, ins >= 2e4))

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

ggplot(geo_ccar_mun20) +
  geom_sf(aes(fill = if_else(is.na(maj), NA, maj4))) +
  geom_sf(data = geo_ccar_mun20_ctr) +
  lbl_largest(geo_ccar_mun20_ctr) +
  map_crs +
  scale_fill_brewer("", palette = "Set2", na.value = "grey50") +
  # scale_fill_viridis_d("", option = "C") +
  theme_map +
  labs(title = "Municipal majorities, 2020",
       subtitle = "Based on largest seat shares",
       caption = str_c("Areas are proportional to electorate size\n",
                       "Labels show cities with 20,000+ registered voters"))

ggsave("data-elections/mun-2020/majority-mun-2020.jpg", width = 8, height = 8)

# kthxbye
