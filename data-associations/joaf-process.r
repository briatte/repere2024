library(countrycode)
library(readxl)
library(tidyverse)

# exported from Google Sheets
assocs <- "data-associations/JOAF-2025-05-22 - JOAF-GROUPS.tsv" %>%
  readr::read_tsv(col_types = cols(.default = "c")) %>%
  filter(!dissolved %in% "1", !dissolved2 %in% "1") %>%
  mutate(date = as.Date(datedeclaration),
         code = if_else(is.na(code_corrected), result_citycode, code_corrected)) %>%
  select(id, DEU:CZE, type = typeAvis, date, code) %>%
  pivot_longer(cols = DEU:CZE, names_to = "natl", values_to = "asso") %>%
  # drop empty rows
  filter(!is.na(asso)) %>%
  # temporarily accept all unchecked/unsure groups as true positives
  mutate(asso = if_else(str_detect(asso, "^[A-Z]|\\?"), "1", asso)) %>%
  # drop false positives
  filter(!asso %in% "0")

stopifnot(assocs$type != "Dissolution")

# descriptives ------------------------------------------------------------

nrow(assocs) # 1462 rows
n_distinct(assocs$code) # 231 city codes

# 528 city-nationality dyads
count(assocs, code, natl, asso) %>%
  nrow()

# Lille = 59350 has 20 groups
filter(assocs, code == "59350") %>%
  distinct(code, natl, asso) %>%
  arrange(natl) %>%
  print(n = Inf)

# compare to 2020 ---------------------------------------------------------

prev <- "data-associations/coded-cases-2020.xlsx" %>%
  readxl::read_excel(guess_max = 10^4) %>%
  filter(codedepartment == "59") %>%
  select(code = codeinsee, allemand:roumain) %>%
  pivot_longer(cols = allemand:roumain,
               names_to = "natl", values_to = "asso") %>%
  filter(!is.na(asso)) %>%
  mutate(natl2 = countrycode::countrycode(natl, "country.name.fr", "iso3c"),
         natl2 = if_else(natl == "neerlandais", "NLD", natl2)) %>%
  select(code, natl = natl2, asso)

nrow(prev) # 123 associations
count(prev, code, natl, sorte = TRUE) # 51 city-natl dyads only

distinct(prev, code, natl) %>%
  add_column(in_2020 = 1) %>%
  full_join(add_column(distinct(assocs, code, natl), in_2024 = 1),
            by = c("code", "natl")) %>%
  count(in_2020, in_2024)

# almost perfect match between 2020 and 2024 data (50 out of 51)
# the single missing case in 2024 is present but in a different city
# (RNA W595043014, city code 59299 in 2020, city code 59656 in 2024)
#
# TODO: ... sort out what to do with the 438 additional cases in 2024

# export ------------------------------------------------------------------

assocs %>%
  distinct(code, natl, asso) %>%
  # append 2020 coding
  left_join(distinct(prev, code, natl) %>%
              add_column(asso2020 = 1), by = c("code", "natl")) %>%
  rename(asso2024 = asso) %>%
  mutate(asso2020 = replace_na(asso2020, 0)) %>%
  arrange(code, natl) %>%
  readr::write_tsv("data-associations/associations.tsv")

# work in progress
