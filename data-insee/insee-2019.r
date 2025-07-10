library(countrycode)
library(tidyverse)

d <- "data-2020/Nord_harmonised_final.dta" %>%
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
  # match names used for 2024 (2020 census) data
  select(code = codec_insee, natl,
         # population
         pop19 = popnat_comm, # dens19 = dens,
         # sex
         sex_m19 = comm_homme, sex_f19 = comm_femme,
         # age distr.
         age_lo19 = comm_age15, age_hi19 = comm_age60,
         # length of residency
         res_lo19 = comm_res_m19, res_hi19 = comm_res_20p,
         # educational att.
         edu_lo19 = comm_n_bac2, edu_hi19 = comm_o_bac2,
         # occup. class
         occ_eo19 = comm_empou, occ_va19 = comm_cs_misc) %>%
  distinct()

stopifnot(!duplicated(distinct(d, code, natl)))

filter(d, !is.na(pop19)) %>%
  mutate_if(is.double, round, digits = 2) %>%
  arrange(code, natl) %>%
  readr::write_tsv("data-insee/insee-2019.tsv")

# kthxbye
