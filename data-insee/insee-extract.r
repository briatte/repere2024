library(countrycode)
library(tidyverse)

# Tableau 0 ---------------------------------------------------------------

tab0 <- "data-insee/PROGEDO avril 2025/Tableau 0.csv" %>%
  readr::read_delim(delim = ";", col_types = "ccd") %>%
  set_names(c("ARM_RESID", "NAT_10", "n0")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID))

stopifnot(!is.na(tab0$ARM_RESID))

# Tableau 1	ARM_RESID 	NAT ------------------------------------------------

tab1 <- "data-insee/PROGEDO avril 2025/Tableau 1.csv" %>%
  readr::read_delim(delim = ";", col_types = "ccd") %>%
  set_names(c("ARM_RESID", "NAT_10", "n1")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID))

stopifnot(!is.na(tab1$ARM_RESID))

# next tables were provided in a non-standard delimited format...

read_insee <- function(x) {

  str_split(x, "\\\"") %>%
    map(str_subset, "\\w+") %>%
    map(matrix, nrow = 1) %>%
    map(as.data.frame) %>%
    bind_rows() %>%
    as_tibble()

}

# Tableau 2	ARM_RESID x	NATx	SEXE -----------------------------------------

# TODO: check that SEXE = 1 is males

tab2 <- "data-insee/PROGEDO avril 2025/Tableau 2.csv" %>%
  readr::read_lines() %>%
  read_insee() %>%
  set_names(c("ARM_RESID", "NAT_10", "SEXE", "IPONDI")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID)) %>%
  mutate(SEXE = if_else(SEXE == 1, "sex_m", "sex_f")) %>%
  pivot_wider(id_cols = c(ARM_RESID, NAT_10),
              names_from = "SEXE", values_from = "IPONDI")

stopifnot(!is.na(tab2$ARM_RESID))

# Tableau 3	ARM_RESID x	NATx	AGE(AGEC) ------------------------------------

tab3 <- "data-insee/PROGEDO avril 2025/Tableau 3.csv" %>%
  readr::read_lines() %>%
  read_insee() %>%
  set_names(c("ARM_RESID", "NAT_10", "AGEC", "IPONDI")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID)) %>%
  mutate(AGEC = if_else(AGEC == 60, "age_60", "age_15")) %>%
  pivot_wider(id_cols = c(ARM_RESID, NAT_10),
              names_from = "AGEC", values_from = "IPONDI")

stopifnot(!is.na(tab3$ARM_RESID))

# Tableau 4	ARM_RESID x	NATx	ANARR ----------------------------------------

tab4 <- "data-insee/PROGEDO avril 2025/Tableau 4.csv" %>%
  readr::read_lines() %>%
  read_insee() %>%
  set_names(c("ARM_RESID", "NAT_10", "ANARR_2", "IPONDI")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID)) %>%
  mutate(ANARR_2 = if_else(ANARR_2 == "20anset+ - Fr",
                           "res_20", "res_19")) %>%
  pivot_wider(id_cols = c(ARM_RESID, NAT_10),
              names_from = "ANARR_2", values_from = "IPONDI")

stopifnot(!is.na(tab4$ARM_RESID))

# Tableau 5	ARM_RESID x	NATx	DIPLR ----------------------------------------

tab5 <- "data-insee/PROGEDO avril 2025/Tableau 5.csv" %>%
  readr::read_lines() %>%
  read_insee() %>%
  set_names(c("ARM_RESID", "NAT_10", "DIPLR_2", "IPONDI")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID)) %>%
  mutate(DIPLR_2 = if_else(DIPLR_2 == "=>Bac+2", "edu_hi", "edu_lo")) %>%
  pivot_wider(id_cols = c(ARM_RESID, NAT_10),
              names_from = "DIPLR_2", values_from = "IPONDI")

stopifnot(!is.na(tab5$ARM_RESID))

# Tableau 6	ARM_RESID x	NATx	CS1 ------------------------------------------

tab6 <- "data-insee/PROGEDO avril 2025/Tableau 6.csv" %>%
  readr::read_lines() %>%
  read_insee() %>%
  set_names(c("ARM_RESID", "NAT_10", "CS1_2", "IPONDI")) %>%
  filter(str_detect(ARM_RESID, "^59\\d{3}")) %>%
  mutate(ARM_RESID = str_squish(ARM_RESID)) %>%
  mutate(CS1_2 = if_else(CS1_2 == "Emp_Ouv", "occ_empl", "occ_else")) %>%
  pivot_wider(id_cols = c(ARM_RESID, NAT_10),
              names_from = "CS1_2", values_from = "IPONDI")

stopifnot(!is.na(tab6$ARM_RESID))

# aggregate ---------------------------------------------------------------

# doing this in the most stupid way possible, sorry
insee <- full_join(tab0, tab1, by = c("ARM_RESID","NAT_10")) %>%
  full_join(tab2, by = c("ARM_RESID","NAT_10")) %>%
  full_join(tab3, by = c("ARM_RESID","NAT_10")) %>%
  full_join(tab4, by = c("ARM_RESID","NAT_10")) %>%
  full_join(tab5, by = c("ARM_RESID","NAT_10")) %>%
  full_join(tab6, by = c("ARM_RESID","NAT_10")) %>%
  # coerce
  mutate(across(n0:occ_empl, ~ round(as.numeric(.x), 2))) %>%
  # recode natl
  mutate(natl = if_else(NAT_10 == "Neerlandais", "Pays-Bas", NAT_10),
         natl = if_else(NAT_10 == "Autre Europe", "Singapour", natl),
         natl = countrycode::countrycode(natl, "country.name.fr", "iso3c"),
         natl = if_else(natl == "SGP", "Other EU", natl)) %>%
  select(code = ARM_RESID, natl, everything(), -NAT_10) %>%
  # not required, being careful
  distinct() %>%
  arrange(code, natl)

readr::write_tsv(insee, "data-insee/insee-extract.tsv")

# work in progress
