# counts of French and other EU nationalities (PT, ES, IT, DE, BE, PL, RO, NL)
# source: census 2021

library(tidyverse)

b <- "https://www.insee.fr/fr/statistiques/tableaux/8202137/"

recode_nat <- function(x) {

  case_match(x, "Ensemble" ~ "Total",
             "Portugais" ~ "PT",
             "Italiens" ~ "IT",
             "Espagnols" ~ "ES",
             "Allemands" ~ "DE",
             "Belges" ~ "BE",
             "Polonais" ~ "PL",
             "Roumains" ~ "RO",
             "Néerlandais" ~ "NL",
             "Autres nationalités de l'UE" ~ "Other EU",
             .default = NA_character_)

}

# France ------------------------------------------------------------------
# https://www.insee.fr/fr/statistiques/8202137?sommaire=8202145&geo=METRO-1

u <- str_c(b, "METRO/1/rp2021_td_nat1.csv")
f <- fs::path("data-nationalities/", fs::path_file(u)) %>%
  str_replace("nat1", "nat1_FR")

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = TRUE)
}

# total
pop_fr <- read_delim(f, delim = ";", skip = 94, n_max = 49,
           col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_fr = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_fr = if_else(nat == "Other EU", n_fr / 18, n_fr),
         prop_fr = if_else(nat == "Total", n_fr, NA)) %>%
  fill(prop_fr, .direction = "up") %>%
  mutate(prop_fr = 100 * n_fr / prop_fr)

# males
pop_fr_m <- read_delim(f, delim = ";", skip = 149, n_max = 49,
                    col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_fr = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_fr = if_else(nat == "Other EU", n_fr / 18, n_fr),
         prop_fr = if_else(nat == "Total", n_fr, NA)) %>%
  fill(prop_fr, .direction = "up") %>%
  mutate(prop_fr = 100 * n_fr / prop_fr)

# females
pop_fr_f <- read_delim(f, delim = ";", skip = 205, n_max = 49,
             col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_fr = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_fr = if_else(nat == "Other EU", n_fr / 18, n_fr),
         prop_fr = if_else(nat == "Total", n_fr, NA)) %>%
  fill(prop_fr, .direction = "up") %>%
  mutate(prop_fr = 100 * n_fr / prop_fr)

# Nord --------------------------------------------------------------------
# https://www.insee.fr/fr/statistiques/8202137?sommaire=8202145&geo=DEP-59

u <- str_c(b, "DEP/59/rp2021_td_nat1.csv")
f <- fs::path("data-nationalities/", fs::path_file(u)) %>%
  str_replace("nat1", "nat1_59")

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = TRUE)
}

# total
pop_59 <- read_delim(f, delim = ";", skip = 94, n_max = 49,
                     col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_59 = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_59 = if_else(nat == "Other EU", n_59 / 18, n_59),
         prop_59 = if_else(nat == "Total", n_59, NA)) %>%
  fill(prop_59, .direction = "up") %>%
  mutate(prop_59 = 100 * n_59 / prop_59)

# males
pop_59_m <- read_delim(f, delim = ";", skip = 149, n_max = 49,
                       col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_59 = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_59 = if_else(nat == "Other EU", n_59 / 18, n_59),
         prop_59 = if_else(nat == "Total", n_59, NA)) %>%
  fill(prop_59, .direction = "up") %>%
  mutate(prop_59 = 100 * n_59 / prop_59)

# females
pop_59_f <- read_delim(f, delim = ";", skip = 205, n_max = 49,
                       col_types = cols(), col_names = FALSE) %>%
  mutate(nat = recode_nat(X1)) %>%
  select(nat, n_59 = X6) %>%
  filter(!is.na(nat)) %>%
  # for other nationalities, take 1 / (EU 27 - FR + 8 nationalities = 18)
  mutate(n_59 = if_else(nat == "Other EU", n_59 / 18, n_59),
         prop_59 = if_else(nat == "Total", n_59, NA)) %>%
  fill(prop_59, .direction = "up") %>%
  mutate(prop_59 = 100 * n_59 / prop_59)

# assemble and export -----------------------------------------------------

bind_rows(
  full_join(pop_fr, pop_59, by = "nat") %>%
    add_column(sex = "Both", .after = 1) %>%
    filter(nat != "Total")
  ,
  full_join(pop_fr_m, pop_59_m, by = "nat") %>%
    add_column(sex = "M", .after = 1) %>%
    filter(nat != "Total")
  ,
  full_join(pop_fr_f, pop_59_f, by = "nat") %>%
    add_column(sex = "F", .after = 1) %>%
    filter(nat != "Total")
) %>%
  mutate_if(is.numeric, round, 3) %>%
  readr::write_tsv("data-nationalities/nationalities.tsv")

# kthxbye
