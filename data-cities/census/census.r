# city-level stats from the 2021 French census
# how it works: https://www.insee.fr/fr/information/2383265

library(cartogram)
library(sf)
library(tidyverse)

# master archive ----------------------------------------------------------
# https://www.insee.fr/fr/statistiques/5359146

f <- "data-cities/census/dossier_complet.zip"

b <- "https://www.insee.fr/fr/statistiques/fichier/5359146/"
u <- str_c(b, fs::path_file(f))

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = FALSE)
}

# data, Nord (59) only ----------------------------------------------------

z <- "data-cities/census/census-stats.rds"

if (!fs::file_exists(z)) {

  # takes around 5 minutes
  cat("Writing in", z, "...\n")
  readr::read_delim(unz(f, "dossier_complet.csv"),
                    delim = ";", locale = locale(decimal_mark = "."),
                    guess_max = 4e4) %>%
    filter(str_detect(CODGEO, "^59")) %>%
    readr::write_rds(z) # 10 MB (.rds) v. 14 MB (.csv)

}

d <- readr::read_rds(z)

# sanity check: all cities reported
stopifnot(nrow(d) == 648)

# metadata, excluding city codes ------------------------------------------

z <- "data-cities/census/census-vars.rds"

if (!fs::file_exists(z)) {

  cat("Writing in", z, "...\n")
  readr::read_csv2(unz(f, "meta_dossier_complet.csv"), col_types = "c") %>%
    filter(str_detect(COD_VAR, "CODGEO", negate = TRUE)) %>%
    readr::write_rds(z)

}

v <- readr::read_rds(z)

# % women -- P21_POPF
str_subset(v$LIB_VAR, "Pop Femmes en 2021")
v$COD_VAR[ str_which(v$LIB_VAR, "Pop Femmes en 2021") ]

# % aged <60 -- P21_POP0014, P21_POP1529, P21_POP3044, P21_POP4559
str_subset(v$LIB_VAR, "Pop (0-14|15-29|30-44|45-59).* en 2021")
v$COD_VAR[ str_which(v$LIB_VAR, "Pop (0-14|15-29|30-44|45-59).* en 2021") ]

# % aged 60+ -- P21_POP6074, P21_POP7589, P21_POP90P
str_subset(v$LIB_VAR, "Pop (60-74|75-89|90 ans).* en 2021")
v$COD_VAR[ str_which(v$LIB_VAR, "Pop (60-74|75-89|90 ans).* en 2021") ]

# % 2+ years of post-secondary education (bac + 2) -- P21_NSCOL15P_SUP2
str_subset(v$LIB_VAR, "bac \\+ 2")
v$COD_VAR[ first(str_which(v$LIB_VAR, "bac \\+ 2")) ]

# % working class -- C21_POP15P_CS5
str_subset(v$LIB_VAR, "Pop.*(vriers|ploy).*2021")
v$COD_VAR[ first(str_which(v$LIB_VAR, "Pop.*(vriers|ploy).*2021")) ]
# % working class -- C21_POP15P_CS5 (employees), C21_POP15P_CS6 (blue collars)
str_subset(v$LIB_VAR, "Pop.*Employé.*2021")
v$COD_VAR[ first(str_which(v$LIB_VAR, "Pop.*Employé.*2021")) ] # CS5
str_subset(v$LIB_VAR, "Pop.*Ouvrier.*2021")
v$COD_VAR[ first(str_which(v$LIB_VAR, "Pop.*Ouvrier.*2021")) ] # CS6

# % residents 20+ years
str_subset(v$LIB_VAR, "même commune")
str_subset(v$LIB_VAR, "habit")

# ...

# final data --------------------------------------------------------------

d <- d %>%
  transmute(
    code = as.character(CODGEO),
    pop = P21_POP0014 + P21_POP1529 + P21_POP3044 + P21_POP4559,
    pop_fem = P21_POPF,
    pop_60 = P21_POP6074 + P21_POP7589 + P21_POP90P,
    pop_bac2 = P21_NSCOL15P_SUP2,
    pop_empl = C21_POP15P_CS5,
    pop_ouvr = C21_POP15P_CS6
  ) %>%
  mutate(
    pop = pop + pop_60,
    pop_fem = 100 * pop_fem / pop,
    pop_60 = 100 * pop_60 / pop,
    pop_bac2 = 100 * pop_bac2 / pop,
    pop_ouvr = 100 * pop_ouvr / pop
  ) %>%
  mutate_if(is.double, round, 2)

# export
readr::write_tsv(d, "data-cities/census/census-stats.tsv")

# compare population to legal population ----------------------------------

# adds city names (used later in maps)
d <- readr::read_tsv("data-cities/population/population.tsv",
                     show_col_types = FALSE) %>%
  mutate(code = as.character(code)) %>%
  full_join(d, by = "code")

with(d, cor(pop, pop21))  # 0.99
with(d, max(pop21 - pop)) # 1832

# a few remarkable cases (threshold: +1,000 or +5%)
filter(d, (pop21 - pop) > 1000 | (pop21 - pop) > (0.05 * pop21)) %>%
  mutate(delta = pop21 - pop) %>%
  arrange(-delta)

# maps --------------------------------------------------------------------

geo <- readr::read_rds("data-cities/geography/city-contours.rds") %>%
  st_transform("EPSG:2154") %>%
  full_join(d, by = "code")

f <- "data-cities/census/contiguous-cartogram-pop21.rds"
if (!fs::file_exists(f)) {

  cat("Computing", f, "...\n")
  cartogram::cartogram_cont(geo, "pop21", itermax = 15, verbose = TRUE) %>%
    readr::write_rds(f)

}

geo_ccar_pop <- readr::read_rds(f)
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

# `pop_fem`
ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = pop_fem)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "% females in 2021", subtitle = "Source: census",
       caption = str_c("Areas are proportional to population size\n",
                       "Labels show cities with 40,000+ residents"))

ggsave("data-cities/census/pop_fem.jpg", width = 8, height = 8)

# `pop_60`
ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = pop_60)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "% aged 60+ in 2021", subtitle = "Source: census",
       caption = str_c("Areas are proportional to population size\n",
                       "Labels show cities with 40,000+ residents"))

ggsave("data-cities/census/pop_60.jpg", width = 8, height = 8)

# `pop_bac2`
ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = pop_bac2)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "% with 2+ years of post-secondary education in 2021",
       subtitle = "Source: census",
       caption = str_c("Areas are proportional to population size\n",
                       "Labels show cities with 40,000+ residents"))

ggsave("data-cities/census/pop_bac2.jpg", width = 8, height = 8)

# `pop_ouvr`
ggplot(geo_ccar_pop) +
  geom_sf(aes(fill = pop_ouvr)) +
  geom_sf(data = geo_ccar_pop_ctr) +
  lbl_largest(geo_ccar_pop_ctr) +
  map_crs +
  scale_fill_viridis_b("", option = "C") +
  theme_map +
  labs(title = "% working class in 2021", subtitle = "Source: census",
       caption = str_c("Areas are proportional to population size\n",
                       "Labels show cities with 40,000+ residents"))

ggsave("data-cities/census/pop_ouvr.jpg", width = 8, height = 8)

# kthxbye
