# export vote shares of all presidential candidates on both rounds
# could have been done in main script, but thought about it later

library(tidyverse)

# round 1
v1 <- readr::read_tsv("data-elections/pre-2022/results-pre-2022-r1.tsv",
                col_types = "ccci") %>%
  mutate(candidate = str_c(str_to_lower(candidate), "_r1") %>%
           str_replace_all("\\s", "_") %>%
           str_replace_all("é", "e")) %>%
  pivot_wider(names_from = "candidate", values_from = "n") %>%
  rowwise() %>%
  mutate(r1 = sum(across(where(is.numeric)))) %>%
  mutate(across(ends_with("_r1"), ~ .x / r1))

# round 2
v2 <- readr::read_tsv("data-elections/pre-2022/results-pre-2022-r2.tsv",
                col_types = "ccci") %>%
  mutate(candidate = str_c(str_to_lower(candidate), "_r2") %>%
           str_replace_all("\\s", "_") %>%
           str_replace_all("é", "e")) %>%
  pivot_wider(names_from = "candidate", values_from = "n") %>%
  rowwise() %>%
  mutate(r2 = sum(across(where(is.numeric)))) %>%
  mutate(across(ends_with("_r2"), ~ .x / r2))

# collate
vs <- full_join(rename(v1, votes_r1 = r1),
                select(rename(v2, votes_r2 = r2), -city),
                by = "code")

# order columns, round and export
vs[, c(1:2, 2 + order(colMeans(vs[ -(1:2) ]), decreasing = TRUE)) ] %>%
  mutate_if(is.numeric, round, 2) %>%
  readr::write_tsv("data-elections/pre-2022/vshares-pre-2022.tsv")

# kthxbye
