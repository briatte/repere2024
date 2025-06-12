library(countrycode)
library(tidyverse)

insee20 <- "data-lists/panel/Nord_harmonised_final.dta" %>%
  haven::read_dta() %>%
  mutate(nat = haven::as_factor(nat)) %>%
  mutate(natl = case_when(nat == "Néerlandaise" ~ "Pays-Bas",
                          nat == "Grecque" ~ "Grèce",
                          nat == "Slovène" ~ "Slovénie",
                          nat == "Suédoise" ~ "Suède",
                          nat == "Tchèque" ~ "Tchéquie",
                          .default = nat),
         natl = countrycode::countrycode(natl, "country.name.fr", "iso3c")) %>%
  # remove 194 rows with nationalities missing
  filter(!is.na(natl)) %>%
  # vaguely match names used for 2024 data
  select(code = codec_insee, natl,
         pop20 = popnat_comm, dens20 = dens,
         sex_m20 = comm_homme, sex_f20 = comm_femme,
         age15_20 = comm_age15, age60_20 = comm_age60,
         res19_20 = comm_res_m19, res20_20 = comm_res_20p,
         edu_lo20 = comm_n_bac2, edu_hi20 = comm_o_bac2,
         occ_else20 = comm_cs_misc, occ_empl20 = comm_empou) %>%
  distinct()

stopifnot(!duplicated(distinct(insee20, code, natl)))

filter(insee20, !is.na(pop20)) %>%
  readr::write_tsv("data-insee/insee2020.tsv")

# kthxbye
