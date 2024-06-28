# export coverage status of cities
# based on fieldwork notes

library(tidyverse)

d <- "data-lists/coverage/LISTE-COMMUNES-NORD-UGLE - EXPORT.tsv" %>%
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

# sanity check: empty status of unobserved cities is unknown
stopifnot(is.na(d$empty[ d$observed == 0 ]))

count(d, observed, empty)

# export for plotting
readr::write_tsv(d, "data-lists/coverage/coverage.tsv")

# kthxbye
