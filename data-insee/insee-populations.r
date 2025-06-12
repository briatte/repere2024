library(tidyverse)

insee <- "data-insee/insee-extract.tsv" %>%
  readr::read_tsv(col_types = "ccdddddddddddd")

count(insee, natl)

# population density ------------------------------------------------------

pop <- "data-cities/population/population.tsv" %>%
  readr::read_tsv(col_types = "ccd")

stopifnot(!is.na(pop$pop21))

# total NNEUC population
pop_etr <- group_by(insee, code) %>%
  summarise(pop_etr = sum(n0, na.rm = TRUE)) %>%
  full_join(pop, by = "code") %>%
  mutate(density = 100 * pop_etr / pop21)

# min 0 max 0.29
summary(pop_etr$density) # as expected, 38 cities are not in the Insee data

arrange(pop_etr, -density)

# by nationality
pop_nat <- select(insee, code, natl, n0) %>%
  full_join(pop, by = "code") %>%
  mutate(density = 100 * n0 / pop21)

summary(pop_nat$density) # 0 to 29 percent, missing for 1,494 dyads

count(pop_nat, natl) # natl = NA when the city is not in the Insee data

filter(pop_nat, is.na(natl)) %>%
  arrange(-pop21) # Gœulzin (59263), Nieurlet (59433), ...

# remove missing cities and 'Other EU'
pop_nat <- drop_na(pop_nat, natl) %>%
  filter(natl != "Other EU")

# compared to 2020 --------------------------------------------------------

insee20 <- readr::read_tsv("data-insee/insee2020.tsv",
                           col_types = "ccdddddddddddd")

count(insee20, natl)

# weighted populations
summary(insee$n0)      # min. 3, max 1010, missing 1456
summary(insee20$pop20) # min. 2, max 1207

# no many-to-many warning = no duplicated city-nationality dyad in 2020 data
compare <- full_join(pop_nat, insee20, by = c("code", "natl")) %>%
  select(code, natl, pop24 = n0, pop20, dens24 = density, dens20)

# 146 low-N dyads present in 2020 but absent in 2024
filter(compare, !is.na(pop20), is.na(pop24))
# 55 cases where population above 2
# 50 cases above 3
#  9 cases above 10 (e.g. Portuguese in Douai, 59178, n = 22.5 in 2020)
filter(compare, is.na(pop24), pop20 > 10)

r <- drop_na(select(compare, code, natl, pop20, pop24, dens20, dens24))
nrow(r) # comparing 389 city-nationality dyads

with(r, cor(pop20, pop24))   # .99
with(r, cor(dens20, dens24)) # .98

# only 37 city-nationality dyads have changed density by ±10%
r %>%
  mutate(diff_pop = abs(pop24 - pop20),
         diff_dens = abs(dens24 - dens20)) %>%
  filter(diff_dens >= 0.1) %>%
  nrow()
  # arrange(-diff_dens) %>%
  # print(n = Inf)

# the compared cities are the 108 cities with 5,000+ residents
r %>%
  left_join(pop, by = "code") %>%
  distinct(code, city, pop21) %>%
  arrange(pop21) # ... Dunkerque, Roubaix, Tourcoing, Lille

# export ------------------------------------------------------------------

compare %>%
  select(code, natl, pop24, dens24, pop20, dens20) %>%
  mutate(across(starts_with("dens"), ~ round(.x, 4))) %>%
  mutate(across(starts_with("pop"), ~ round(.x, 2))) %>%
  readr::write_tsv("data-insee/insee-populations.tsv")

# work in progress
