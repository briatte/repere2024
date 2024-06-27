#
#

library(tidyverse)

d <- "data-lists/fieldwork/LISTE-COMMUNES-NORD-UGLE - EXPORT.tsv" %>%
  read_tsv(col_types = "cccccii") %>%
  # based on fieldwork notes
  mutate(observed = !str_detect(missing, "manquante|non traitée"),
         empty = str_detect(missing, "^tous")) %>%
  mutate_if(is.logical, as.integer) %>%
  mutate(observed = replace_na(observed, 1),
         empty = case_when(
           observed == 1 & is.na(empty) ~ 0L,
           observed == 0 ~ NA_integer_,
           .default = empty)) %>%
  select(code, city, observed, empty, in_m1 = m1, in_m4 = m4)

count(d, observed, empty)

d

# export for plotting
readr::write_tsv(d, "data-lists/fieldwork/coverage.tsv")

# Relationship to electorate size -----------------------------------------

table(d$complete)
round(100 * prop.table(table(d$complete)), 1)

# covered cities
sum(d$complete != "Unobserved, missing") # 580
round(100 * sum(d$complete != "Unobserved, missing") / nrow(d), 1) # 89.5%

d <- d %>%
  full_join("data-cities/electorate/electorate-counts.tsv" %>%
              read_tsv(col_types = cols()) %>%
              mutate(code = as.character(code), n_ins1000 = n_ins / 1000) %>%
              select(-city),
            by = "code")

# covered voters
sum(d$n_ins[ d$complete != "Unobserved, missing" ]) # 1,757,771
round(100 * sum(d$n_ins[ d$complete != "Unobserved, missing" ]) /
        sum(d$n_ins), 1) # 95.7%

median(d$n_ins[ d$complete == "Observed" ]) # 1,419
median(d$n_ins[ d$complete != "Observed" ]) # 618

median(d$n_ins[ d$complete != "Unobserved, missing" ]) # 1,018
median(d$n_ins[ d$complete == "Unobserved, missing" ]) # 785

median(d$n_ins[ d$complete == "Observed but empty" ]) # 538

# cities with n_eur == 0 should be 'Unobserved but empty'
table(d$complete, d$n_eur == 0)
100 * prop.table(table(d$complete, d$n_eur == 0), 1)

# ... missing 61 + 62 = 123 cities with EU voters (18% of cities)
# ... share of non-French electorate is negligible, quasi-complete data:
sum(d$n_eur[ d$complete != "Observed" & d$n_eur > 0 ]) # 2,080
2080 / sum(d$n_eur) # 0.1%

# ... 16 weird cases where n_eur == 0 but status is 'Observed'
# TODO: check

# cities with n_eur > 0 should be 'Observed'
table(d$complete, d$n_eur > 0)
100 * prop.table(table(d$complete, d$n_eur > 0), 1) # 96% covered

xy <- bind_cols(d, as_tibble(st_coordinates(ctr))) %>%
  mutate(upper = Y > median(xy$Y))

glm(complete == "Unobserved, missing" ~ upper + n_ins, data = xy) %>%
  coef()

# Relationship to 2020 model samples --------------------------------------

table(d$complete, d$m1)
table(d$complete, d$m4)
