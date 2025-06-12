library(countrycode)

assocs_eu <- "data-lists/panel/Nord_harmonised_final.dta" %>%
  haven::read_dta() %>%
  mutate(nat = haven::as_factor(nat)) %>%
  mutate(natl = case_when(nat == "Néerlandaise" ~ "Pays-Bas",
                          nat == "Grecque" ~ "Grèce",
                          nat == "Slovène" ~ "Slovénie",
                          nat == "Suédoise" ~ "Suède",
                          nat == "Tchèque" ~ "Tchéquie",
                          .default = nat),
         natl = countrycode::countrycode(natl, "country.name.fr", "iso3c")) %>%
  distinct(codec_insee, natl, assoc_eur)

stopifnot(!is.na(assocs_eu$assoc_eur))
stopifnot(assocs_eu$assoc_eur %in% 0:1)

# never changes per city
group_by(assocs_eu, codec_insee) %>%
  summarise(n_values = n_distinct(assoc_eur)) %>%
  filter(n_values != 1)

# TODO: update using JOAF 2024?
assocs_eu %>%
  distinct(codec_insee, assoc_eur) %>%
  readr::write_tsv("data-associations/assoc-eu.tsv")

# work in progress
