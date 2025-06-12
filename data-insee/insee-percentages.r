library(tidyverse)

insee <- "data-insee/insee-extract.tsv" %>%
  readr::read_tsv(col_types = "ccdddddddddddd")

# descriptives ------------------------------------------------------------

nrow(insee)        # 2342 rows
count(insee, natl, sort = TRUE)

n_distinct(insee$code) # 610 cities (out of 648, missing 38 cities)

# only 887 rows (city-nationality dyads) with non-missing data
insee %>%
  rowwise() %>%
  filter(any(!is.na(c_across(n0:occ_else))))

# cities with non-missing data
n_distinct(insee$code[ !is.na(insee$n0) ]) # 385 (all cities)
n_distinct(insee$code[ !is.na(insee$n1) ]) # 108 (cities > 5,000)

hab5000 <- unique(insee$code[ !is.na(insee$n1) ])

# same data for 2020 ------------------------------------------------------

insee20 <- readr::read_tsv("data-insee/insee2020.tsv",
                           col_types = "ccdddddddddddd") %>%
  filter(!is.na(pop20))

nrow(insee20)        # 535 rows (all with non-missing data)
count(insee20, natl)

n_distinct(insee20$code) # 108 cities (cities > 5,000)

# very minor differences in comparable cities
hab5000[ !hab5000 %in% insee20$code ] # Petite-Forêt, 5,097 in 2021
insee20$code[ !insee20$code %in% hab5000 ] # Lambres-lez-Douai, 5,011 in 2021

compare <- right_join(insee, insee20, by = c("code", "natl"))

# very little additional data in 2024 when compared to 2020
anti_join(insee, insee20, by = c("code", "natl")) %>%
  filter(natl != "Other EU") %>%
  rowwise() %>%
  # 52 dyads with detailed data (beyond weighted population counts)
  filter(any(!is.na(c_across(sex_m:occ_else))))

# total populations -------------------------------------------------------

with(compare, cor(pop20, n1, use = "complete")) # .99

# 146 dyads for which we got no population data in 2024 but did in 2020
# TODO: inquire
compare %>%
  select(code, natl, n0, n1, pop20) %>%
  filter(is.na(n0))

# some missing groups are rather large
# TODO: inquire
# e.g. Portuguese in Aniche, 59008, n = 11.0 in 2020, total pop. 10,057
# e.g. Portuguese in Douai, 59178, n = 22.5 in 2020, total pop. 40,474
compare %>%
  select(code, natl, n0, n1, pop20) %>%
  filter(is.na(n0), pop20 > 10)

# check: sex --------------------------------------------------------------

summary(insee$sex_m) # 3.5-539    missing 2098
summary(insee$sex_f) # 3.8-471    missing 2099

# no errors
insee %>%
  filter(replace_na(sex_m, 0) + replace_na(sex_f, 0) > n0 + 0.05)

sum(!is.na(compare$sex_m) & !is.na(compare$sex_m20)) # 224
sum(is.na(compare$sex_m) & !is.na(compare$sex_m20))  # 311 more values
with(compare, cor(sex_m, sex_m20, use = "complete")) # .988

sum(!is.na(compare$sex_f) & !is.na(compare$sex_f20)) # 224
sum(is.na(compare$sex_f) & !is.na(compare$sex_f20))  # 311 more values
with(compare, cor(sex_f, sex_f20, use = "complete")) # .988

# large groups missing from 2024
filter(compare, !is.na(sex_f20), is.na(sex_f), sex_f20 > 10) %>%
  select(code:n1, starts_with("sex_"), pop20) %>%
  arrange(-sex_f20)

# example problematic case: Poles from Roubaix (59512, pop. 99,300+)
# POL local pop. = 18.5 (down from 48.2), but missing sex rates?!
# TODO: inquire

# check: age groups -------------------------------------------------------

summary(insee$age_15) # 3.5-541   missing 2087
summary(insee$age_60) # 3.9-501   missing 2164

# no errors
insee %>%
  filter(replace_na(age_15, 0) + replace_na(age_60, 0) > n0 + 0.05)

sum(!is.na(compare$age_15) & !is.na(compare$age15_20)) # 214
sum(is.na(compare$age_15) & !is.na(compare$age15_20))  # 321 more values
with(compare, cor(age_15, age15_20, use = "complete")) # .983

sum(!is.na(compare$age_60) & !is.na(compare$age60_20)) # 175
sum(is.na(compare$age_60) & !is.na(compare$age60_20))  # 360 more values
with(compare, cor(age_60, age60_20, use = "complete")) # .992

# large groups missing from 2024
filter(compare, !is.na(age60_20), is.na(age_60), age60_20 > 10) %>%
  select(code:n1, starts_with("age")) %>%
  arrange(-age60_20)

# check: residents --------------------------------------------------------

summary(insee$res_19) # 3.4-500 missing 2136
summary(insee$res_20) # 3.7-826 missing 2135

# no errors
insee %>%
  filter(replace_na(res_19, 0) + replace_na(res_20, 0) > n0 + 0.05)

sum(!is.na(compare$res_19) & !is.na(compare$res19_20)) # 164
sum(is.na(compare$res_19) & !is.na(compare$res19_20))  # 371 more values
with(compare, cor(res_19, res19_20, use = "complete")) # .991

sum(!is.na(compare$res_20) & !is.na(compare$res20_20)) # 196
sum(is.na(compare$res_20) & !is.na(compare$res20_20))  # 339 more values
with(compare, cor(res_20, res20_20, use = "complete")) # .995

# check: education groups -------------------------------------------------

summary(insee$edu_lo) # 3.5-966   missing 2116
summary(insee$edu_hi) # 3.8-284   missing 2196

# no errors
insee %>%
  filter(replace_na(edu_lo, 0) + replace_na(edu_hi, 0) > n0 + 0.05)

sum(!is.na(compare$edu_lo) & !is.na(compare$edu_lo20)) # 196
sum(is.na(compare$edu_lo) & !is.na(compare$edu_lo20))  # 339 more values
with(compare, cor(edu_lo, edu_lo20, use = "complete")) # .991

sum(!is.na(compare$edu_hi) & !is.na(compare$edu_hi20)) # 135
sum(is.na(compare$edu_hi) & !is.na(compare$edu_hi20))  # 400 more values
with(compare, cor(edu_hi, edu_hi20, use = "complete")) # .985

# check: occupation groups ------------------------------------------------

summary(insee$occ_empl) # 7.0-449   missing 2197
summary(insee$occ_else) # 4.2-272   missing 2202

# TODO: fix 4 errors
insee %>%
  filter(replace_na(occ_empl, 0) + replace_na(occ_else, 0) > n0) %>%
  select(code, natl, n0, starts_with("occ_"))

sum(!is.na(compare$occ_empl) & !is.na(compare$occ_empl20)) # 131
sum(is.na(compare$occ_empl) & !is.na(compare$occ_empl20))  # 404 more values
with(compare, cor(occ_empl, occ_empl20, use = "complete")) # .990

sum(!is.na(compare$occ_else) & !is.na(compare$occ_else20)) # 126
sum(is.na(compare$occ_else) & !is.na(compare$occ_else20))  # 409 more values
with(compare, cor(occ_else, occ_else20, use = "complete")) # .986


# join to registr. rates --------------------------------------------------

reg <- readr::read_tsv("data-voters/voter-registration.tsv",
                       col_types = "ccd") %>%
  inner_join(insee20, by = c("code", "natl"))

pop <- "data-cities/population/population.tsv" %>%
  readr::read_tsv(col_types = "ccddddd") %>%
  select(code, pop21) %>%
  mutate(city_pop = cut(pop21, c(5000, 7499, 9999, 19999, Inf),
             right = FALSE, dig.lab = 9))

reg <- left_join(reg, pop, by = "code")

asso <- "data-associations/associations.tsv" %>%
  read_tsv(col_types = "ccii") %>%
  mutate(asso = asso2020)

reg <- left_join(reg, asso, by = c("code", "natl"))

# 198 rows
reg

reg %>%
  mutate(natl = forcats::fct_relevel(natl, "BEL", after = 0)) %>%
  fixest::feols(reg24 ~ occ_empl20 + res20_20 + edu_hi20 + age60_20 +
                sex_f20 + factor(natl) + dens20 + city_pop,
              data = .) %>%
  summary(cluster = ~ natl) %>%
  modelsummary::modelsummary(output = "reg24.docx", stars = TRUE)

# //Inscription//
# front2 tc3 i.candnat  part_m2020 ib2.orient lepen assoc_nat1 assoc_eur, family(gaussian) cluster(nat2)

# //Inscription//
#   glm tinscrit2 empou res20 bac2 age60 femme ib2.nat2 dens front2 tc3 i.candnat  part_m2020 ib2.orient lepen assoc_nat1 assoc_eur, family(gaussian) cluster(nat2)


# percentages -------------------------------------------------------------

pct <- insee %>%
  # NA = missing (weighted n < 3)
  mutate(across(sex_m:occ_empl, ~ round(100 * .x / n0, 2))) %>%
  rename_if(is.numeric, ~ str_c("pct_", .x)) %>%
  # mutate_all(replace_na, 0) %>%
  # mutate(pct_n1 = na_if(pct_n1, 0)) %>%
  select(code, natl, n0 = pct_n0, n1 = pct_n1, everything())

readr::write_tsv(pct, "data-insee/insee-percentages.tsv")

# work in progress
