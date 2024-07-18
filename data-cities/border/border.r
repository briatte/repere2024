# compare all `front*` variables present in the 2020 datasets, select the right
# ones to denote border status, and add adjacency to border city status
#
# NOTE: requires exported columns from two author-provided Stata datasets

library(sf)
library(tidyverse)

# merge 2020 datasets and sort out variables ------------------------------

# Stata code to export relevant columns from source:
#
# outsheet codec_insee nat_9 front front2 front3 using ///
#     "border_from_59_inscription_communes.tsv"
#
a <- "data-cities/border/border_from_59_inscription_communes.dta.tsv" %>%
  readr::read_tsv(show_col_types = FALSE)

# no duplicated rows
distinct(a) %>%
  group_by(codec_insee, nat_9) %>%
  filter(length(codec_insee) > 1)

# Stata code to export relevant columns from source:
#
# outsheet codec_insee nat_9 front* using ///
#     "border_from_Nord_harmonised_final.dta.tsv"
#
b <- "data-cities/border/border_from_Nord_harmonised_final.dta.tsv" %>%
  readr::read_tsv(show_col_types = FALSE)

# no duplicated rows
distinct(b) %>%
  group_by(codec_insee, nat_9) %>%
  filter(length(codec_insee) > 1)

# one-to-one merge
d <- distinct(full_join(a, b, by = c("codec_insee", "nat_9")))

# variable names differ: front2.x == front3.y
filter(d, front2.x != front3.y)
# variable names differ: front3.x == front2.y
filter(d, front3.x != front2.y)

# remove duplicated columns
d <- select(d, -front2.y, -front3.y)

# coding of `front` -------------------------------------------------------

# variable `front` does not vary by city
distinct(a) %>%
  group_by(codec_insee) %>%
  filter(n_distinct(front) > 1) # 0

# equal to 1 for 416 cities
distinct(a, codec_insee, front) %>%
  count(front)

border1 <- distinct(a, codec_insee, front)

# coding of `front2` ------------------------------------------------------

with(a, table(nat_9, front2)) # = 1 only for Belgians in 74 cities

# (Kelbel et al. 2024 claim n = 76 at pages 7 and 8)

border2 <- select(distinct(a), codec_insee, nat_9, front2) %>%
  mutate(be = if_else(nat_9 == "Belges", "front2_be", "front2")) %>%
  distinct(codec_insee, front2, be) %>%
  pivot_wider(names_from = "be", values_from = "front2")

table(border2$front2, exclude = NULL)    # always 0, NA = 115
table(border2$front2_be, exclude = NULL) # = 1 in 74, NA = 82

# coding of `front3` ------------------------------------------------------

# variable `front3` does not vary by city
distinct(a) %>%
  group_by(codec_insee) %>%
  filter(n_distinct(front3) > 1)

# equal to 1 for 74 cities
distinct(a, codec_insee, front3) %>%
  count(front3)

border3 <- distinct(a, codec_insee, front3)

border <- full_join(border1, border2, by = "codec_insee") %>%
  full_join(border3, by = "codec_insee")

# `front3` = `front2_be`
filter(border, front3 != front2_be)
filter(border, front3 != front2_be | is.na(front2_be))
filter(mutate(border, front2_be = replace_na(front2_be, 0)), front3 != front2_be)

# confirm coding, remove unused columns, finalize
border <- mutate(border, code = as.character(codec_insee)) %>%
  select(code, front, front3)

# final columns -----------------------------------------------------------

with(border, table(front, front3, exclude = NULL))

# weird cases
filter(border, front == 0, front3 == 1)
filter(a, codec_insee %in% c(59315, 59530)) %>%
  knitr::kable()

# maps --------------------------------------------------------------------

geo <- readr::read_rds("data-cities/geography/city-contours.rds") %>%
  full_join(border, by = "code")

ctr <- readr::read_rds("data-cities/geography/city-centres.rds")

map_crs <- coord_sf(crs = "EPSG:9850") # RGF93 v2b / CC50

theme_map <- theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "inside", legend.position.inside = c(0.2, 0.2))

# `front` alone
ggplot(geo) +
  geom_sf(aes(fill = str_c("front = ", front))) +
  map_crs +
  scale_fill_viridis_d("", option = "C") +
  theme_map +
  labs(title = "Border cities: variable `front`",
       subtitle = "Source: 59_inscription_communes.dta",
       caption = "Variable not used in any of the models of Kelbel et al. 2024")

ggsave("data-cities/border/border-front.jpg", width = 8, height = 8)

# `front3` alone
ggplot(geo) +
  geom_sf(aes(fill = str_c("front3 = ", front3))) +
  map_crs +
  scale_fill_viridis_d("", option = "C") +
  theme_map +
  labs(title = "Border cities: variable `front3`",
       subtitle = "Source: 59_inscription_communes.dta",
       caption = str_c("Variable used in all models of Kelbel et al. 2024\n",
                       "Models 1, 3, 4: used as such for all nationalities\n",
                       "Models 2, 5, 6: restricted to Belgian nationals"))

ggsave("data-cities/border/border-front3.jpg", width = 8, height = 8)

# centroids of weird cases
ctr_weird <- full_join(ctr, border, by = "code") %>%
  filter(front == 0, front3 == 1)

ggplot(geo) +
  geom_sf(aes(fill = str_c("front = ", front, ", front3 = ", front3))) +
  ggrepel::geom_label_repel(
    data = ctr_weird,
    aes(label = code, geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 0.7, max.overlaps = Inf,
    min.segment.length = 0, seed = 202459
  ) +
  geom_sf(data = ctr_weird) +
  map_crs +
  scale_fill_viridis_d("", option = "C") +
  theme_map +
  labs(title = "Border city status: variables `front` x `front3`",
       subtitle = "Source: 59_inscription_communes.dta")

ggsave("data-cities/border/border-front-front3.jpg", width = 8, height = 8)

# fix missing values ------------------------------------------------------

# after visual inspection, there two cities at the border that should be coded
# as contiguous to Belgium; all other cities with no status should be coded 0

# the 2 missing values explain why Kelbel et al. 2024 report 76 cities coded
# as located at the Belgian border, whereas the dataset contains only 74 such
# cases; the reason why the 2 cities are missing from the data is likely to be
# that neither had any EU voter on their electoral lists

border <- add_column(border, city = NA_character_) %>%
  add_row(code = "59217", front = NA, front3 = NA, city = "Eth") %>%
  add_row(code = "59004", front = NA, front3 = NA, city = "Aix-en-Pévèle")

# centroids of missing cases
ctr_missing <- full_join(ctr, border, by = "code") %>%
  filter(!is.na(city))

ggplot(geo) +
  geom_sf(aes(fill = str_c("front3 = ", front3))) +
  ggrepel::geom_label_repel(
    data = ctr_missing,
    aes(label = if_else(is.na(city), NA, city), geometry = geometry),
    stat = "sf_coordinates",
    box.padding = 0.7, max.overlaps = Inf,
    min.segment.length = 0, seed = 202459
  ) +
  geom_sf(data = ctr_missing) +
  map_crs +
  scale_fill_viridis_d("", option = "C") +
  theme_map +
  labs(title = "Border city status: variable `front3`",
       subtitle = "Source: 59_inscription_communes.dta",
       caption = "Labels show cities that should be coded as 1")

ggsave("data-cities/border/border-front3-missing.jpg", width = 8, height = 8)

# adjacent cities ---------------------------------------------------------

# fix `front3` and fill missing values
geo <- mutate(geo, front3 = case_when(code %in% c("59004", "59217") ~ 1,
                                      !is.na(front3) ~ front3,
                                      .default = 0))

# cities adjacent to border cities
adj <- filter(geo, front3 == 1) %>%
  sf::st_filter(geo, ., .predicate = st_touches) %>%
  # remove border cities
  filter(front3 == 0) %>%
  pull(code)

geo <- geo %>%
  mutate(adjacent = as.integer(code %in% adj),
         border_status = str_c(front3, adjacent) %>%
           factor(labels = c("None", "Adjacent to border city",
                             "At Belgian border")))

# n = 76 at Belgian border, n = 87 adjacent to border city
count(as_tibble(geo), border_status)

# map shows only 2-3 inconsistencies
ggplot(geo) +
  geom_sf(aes(fill = border_status)) +
  scale_fill_viridis_d("", option = "C") +
  map_crs +
  theme_map +
  labs(title = str_c("Border city status: variables `front3` (corrected)",
                     " and `adjacent`"),
       subtitle = "Source: 59_inscription_communes.dta and personal additions")

ggsave("data-cities/border/border-front3-adjacent.jpg", width = 8, height = 8)

# export ------------------------------------------------------------------

# generate complete listing of city codes, with corrected `front3` variable
# (undecided yet as to what to do with `front`, which was apparently coded by
# an intern and never used in the final analysis; dropping it below)
d <- as_tibble(select(geo, code, front, front3, adjacent)) %>%
  select(-geometry) %>%
  # add city names for good measure
  left_join(readr::read_tsv("data-cities/population/population.tsv",
                            col_types = "ccd"),
            by = "code") %>%
  select(code, city, border = front3, adjacent)

# final sanity checks
stopifnot(nrow(d) == 648)
stopifnot(!is.na(d$border) & !is.na(d$adjacent))
stopifnot(d$border + d$adjacent < 2)

readr::write_tsv(d, "data-cities/border/border.tsv")

# kthxbye
