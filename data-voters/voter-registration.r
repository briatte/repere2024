library(countrycode)
library(tidyverse)

v <- "data-voters/Listes-59-2024-07-09-REPERE-CODING - VOTERS.tsv" %>%
  readr::read_tsv(col_types = cols(.default = "c")) %>%
  filter(vote != "(mun only)") %>%
  mutate(liste_muni = if_else(liste_muni == "?", NA_character_, liste_muni)) %>%
  mutate(vote = case_when(vote %in% c("1", "/1", "//1", "2") ~ 1,
                          vote %in% c("0", "/0", "//0") ~ 0,
                          vote %in% c("3", "4", ".") ~ NA),
         natl = countrycode::countrycode(`identifiant nationalité`,
                                         "iso2c", "iso3c"))

count(v, liste_euro, liste_muni)

count(v, vote)
count(v, `identifiant nationalité`, natl)

voters_by_city <- filter(v, liste_euro == "1") %>%
  group_by(code, natl) %>%
  summarise(
    n_voters = sum(!is.na(vote)),
    n_missing = sum(is.na(vote)),
    n_total = n_voters + n_missing
  )

reg <- "data-insee/insee-populations.tsv" %>%
  readr::read_tsv(col_types = "ccddddd") %>%
  select(-starts_with("dens")) %>%
  left_join(voters_by_city, by = c("code", "natl")) %>%
  # restrict to cities with known NNEUC population(s)
  filter(!is.na(pop20) | !is.na(pop24)) %>%
  mutate(reg24 = 100 * n_total / pop24,
         reg20 = 100 * n_total / pop20)

filter(reg, pop24 >= 20, pop20 >= 20) # 194
filter(reg, pop24 >= 20) # 277

sum(!is.na(reg$reg24) & reg$pop24 >= 20) # 271
sum(!is.na(reg$reg20) & reg$pop20 >= 20) # 224

# using the previous population estimate would solve 0 large case, relative to
# either the observed voting population or the estimated NNEUC population
filter(reg, !is.na(reg20), is.na(reg24)) %>%
  filter(pop20 > 10) # showing only largest cases

# n = 3 problematic cases where the registr. rate is clearly above 100% when
# using the new population estimate, and less above 100% when using the old
# one -- in these cases, let's use the old one, and cap the rates at 100%
# TODO: document
filter(reg, pop24 >= 20, n_total > pop24) %>%
  arrange(-n_voters)

reg <- reg %>%
  filter(pop24 >= 20, !is.na(n_total)) %>%
  mutate(reg24 = case_when(reg24 > 100 & reg20 < reg24 ~ reg20,
                           .default = reg24),
         reg24 = if_else(reg24 > 100, 100, reg24)) # n = 271

sum(!is.na(reg$reg24)) # 271
summary(reg$reg24) # 0.8-100%, mean = median ~ 45.4
sd(reg$reg24) # 20.6

ggplot(reg, aes(x = reg24)) +
  geom_dotplot() +
  facet_wrap(~ natl, scales = "free")

ggsave("data-voters/voter-registration.png", width = 12, height = 8)

reg %>%
  mutate(reg24 = round(reg24, 2)) %>%
  select(code:natl, reg24) %>%
  readr::write_tsv("data-voters/voter-registration.tsv")

# work in progress
