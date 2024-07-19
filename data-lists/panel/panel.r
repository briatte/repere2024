# try to match voters observed in both 2020 and 2024, using 6 different methods
# requires the Stata dataset containing the 2020 sample

library(tidyverse)

# 2024 sample -------------------------------------------------------------

e <- "data-lists/electorate/electorate-eur.rds" %>%
  read_rds() %>%
  select(fam1, first, dob, pid, code, female)

# `pid` never repeats, except once for 2 voters who are registered in two
# different cities (see `electorate/electorate-problematic-cases-2.tsv`)

filter(e, pid %in% pid[ duplicated(e$pid )]) %>%
  arrange(fam1)

# 2020 sample -------------------------------------------------------------

p <- haven::read_dta("data-lists/panel/Nord_harmonised_final.dta") %>%
  select(
    # variables also available in 2024: city + personal info + some demographics
    city20 = lcom, code20 = codec_insee,
    fam1 = nomdenaissance, first = prenom, dob = ndate,
    female = sexe, nat, born = codepaysn,
    # additional variables: participation in 2020 municipal election
    t1, t2
  ) %>%
  mutate(dob = as.character(dob),
         dob = str_replace(dob, "(\\d{4})-(\\d{2})-(\\d{2})", "\\3/\\2/\\1"),
         female = as.integer(female), # 0 = male, 1 = female
         # missing values are encoded as "" in `fam1` and `first`
         fam1 = na_if(fam1, ""), first = na_if(first, ""))

# city codes: never missing, ...
stopifnot(!is.na(p$code20))

# ... but neither perfectly stable across years
setdiff(unique(p$code20), unique(e$code))
setdiff(unique(e$code), unique(p$code20))

# missing values in personal info (first pass)
sum(is.na(p$dob))   # 207
sum(is.na(p$fam1))  # 194
sum(is.na(p$first)) # 196

# completely unusable
sum(is.na(p$fam1) & is.na(p$first) & is.na(p$dob)) # 194
# no voting data anyway
count(filter(p, is.na(fam1), is.na(first), is.na(dob)), t1)
count(filter(p, is.na(fam1), is.na(first), is.na(dob)), t2)

# 2020 election
count(p, t1) # effectively missing: NA = 971,   3 = 43, 4 = 802
count(p, t2) # effectively missing: NA = 8,635, 3 = 11, 4 = 296

# sanity check: round 2 always missing if round 1 is
count(filter(p, is.na(t1)), t2)

# drop unidentified voters and those with no known voting record
p <- filter(p, !(is.na(fam1) & is.na(first) & is.na(dob)) & !is.na(t1))

nrow(p) # left to match: 10,852

# missing values in personal info (second last pass)
sum(is.na(p$dob))   # 10
sum(is.na(p$fam1))  # 0
sum(is.na(p$first)) # 2

# missing values in demographics (none)
sum(is.na(p$female)) # 0
sum(is.na(p$nat))    # 0
sum(is.na(p$born))   # 0

# [1] pid = fam1, first, dob ----------------------------------------------
#
# same identifiers used for 2024, except we also encode missing dates of birth
# as literal "NA" to avoid duplicated hashes

p1 <- mutate(p, first = str_replace_na(first), dob = str_replace_na(dob)) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup() %>%
  select(code20, pid, fam1, first, female, dob, t1, t2)

# no duplicated `pid`
stopifnot(!duplicated(p1$pid))

e1 <- full_join(e, p1, by = "pid")

table(is.na(e1$code20)) # 2024 sample: 1,978 not found in 2020

sum(is.na(e1$code20)) / sum(!is.na(e1$code))   # 19% of 2024 sample
sum(is.na(e1$code20)) / sum(!is.na(e1$code20)) # 18% of 2020 sample

table(is.na(e1$code))   # 2020 sample: 2,467 not found in 2024

sum(is.na(e1$code)) / sum(!is.na(e1$code20))   # 22% of 2020 sample
sum(is.na(e1$code)) / sum(!is.na(e1$code))     # 24% of 2024 sample (almost)

# continue with 2024 sample that was not matched
e2 <- filter(e1, is.na(code20)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".y"), -code20, -t1, -t2) %>%
  rename_with(~ str_remove(.x, ".x$"))

nrow(e2) # 1,978 left to match by other means

# continue with 2020 sample that was not matched
p2 <- filter(e1, is.na(code)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".x"), -code, -pid) %>%
  rename_with(~ str_remove(.x, ".y$"))

nrow(p2) # 2,467 left to match by other means

# [2] pid = code, fam1, dob, female ----------------------------------------
#
# alternative pid by code + fam1 + dob + sex (to allow varying first names)
# sex allows to de-duplicate one voter

e2 <- rowwise(e2) %>%
  mutate(pid2 = rlang::hash(str_c(code, fam1, dob, female))) %>%
  ungroup()

stopifnot(!duplicated(e2$pid2))
# e2[ duplicated(e2$pid2), ]

p2 <- rowwise(p2) %>%
  mutate(pid2 = rlang::hash(str_c(code20, fam1, dob, female))) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(p2$pid2))

e2 <- full_join(e2, p2, by = "pid2")

table(is.na(e2$code20)) # 2024 sample: 1,880 still not found in 2020
table(is.na(e2$code))   # 2020 sample: 2,369 still not found in 2024

# continue with 2024 sample that was not matched
e3 <- filter(e2, is.na(code20)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".y"), -code20, -t1, -t2) %>%
  rename_with(~ str_remove(.x, ".x$"))

nrow(e3) # 1,880 left to match by other means

# continue with 2020 sample that was not matched
p3 <- filter(e2, is.na(code)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".x"), -code, -pid, -pid2) %>%
  rename_with(~ str_remove(.x, ".y$"))

nrow(p3) # 2,369 left to match by other means

# [3] pid = code, fam1, first ---------------------------------------------
#
# alternative pid by code + fam1 + first (to allow for missing `dob` in 2020)

e3 <- rowwise(e3) %>%
  mutate(pid3 = rlang::hash(str_c(code, fam1, first))) %>%
  ungroup()

# allow duplicates, ...
# stopifnot(!duplicated(e3$pid3))

# ... since the two cases are very likely to be the same voters, with slightly
# different personal information (mistakes in dates of birth?)
e3[ e3$pid3 %in% e3$pid3[ duplicated(e3$pid3) ], ]

p3 <- rowwise(p3) %>%
  mutate(pid3 = rlang::hash(str_c(code20, fam1, first))) %>%
  ungroup()

# this creates 1 duplicate in the 2020 data, ...
filter(p3, pid3 %in% p3$pid3[ duplicated(p3$pid3) ])

# ... but the duplicate does not exist in the 2024 data left to match, ...
filter(p3, pid3 %in% p3$pid3[ duplicated(p3$pid3) ]) %>%
  inner_join(e3, by = "pid3")

# ... so we just drop one of the two rows arbitrarily
p3 <- filter(p3, !duplicated(pid3))

# no duplicated `pid` (which means that the two previous likely duplicates are
# not going to be matched, hence no many-to-one warning message)
stopifnot(!duplicated(p3$pid3))

e3 <- full_join(e3, p3, by = "pid3")

table(is.na(e3$code20)) # 2024 sample: 1,869 still not found in 2020
table(is.na(e3$code))   # 2020 sample: 2,357 still not found in 2024

# continue with 2024 sample that was not matched
e4 <- filter(e3, is.na(code20)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".y"), -code20, -t1, -t2) %>%
  rename_with(~ str_remove(.x, ".x$"))

nrow(e4) # 1,869 left to match by other means

# continue with 2020 sample that was not matched
p4 <- filter(e3, is.na(code)) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".x"), -code, -pid2, -pid3) %>%
  rename_with(~ str_remove(.x, ".y$"))

nrow(p4) # 2,357 left to match by other means

# [4] pid = code, fam1 (when unique to code) ------------------------------
#
# note: not risky, since family name is never missing in that subsample

# n = 1,767 remaining voters have a unique family name at the city-level
group_by(e4, code) %>%
  add_count(fam1, name = "n_fam1") %>%
  filter(n_fam1 == 1)

e4 <- group_by(e4, code) %>%
  add_count(fam1, name = "n_fam1") %>%
  rowwise() %>%
  mutate(pid4 = rlang::hash(str_c(code, fam1)),
         pid4 = if_else(n_fam1 == 1, pid4, NA_character_)) %>%
  select(-n_fam1) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(e4$pid4)))

p4 <- group_by(p4, code20) %>%
  add_count(fam1, name = "n_fam1") %>%
  rowwise() %>%
  mutate(pid4 = rlang::hash(str_c(code20, fam1)),
         pid4 = if_else(n_fam1 == 1, pid4, NA_character_)) %>%
  select(-n_fam1) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(p4$pid4)))

# this time, we have to keep only those rows with a valid `pid4`
e4_match <- filter(e4, !is.na(pid4)) %>%
  full_join(filter(p4, !is.na(pid4)), by = "pid4") %>%
  filter(!is.na(code20), !is.na(code)) # keep only matches

nrow(e4_match) # 13 matches
# arrange(select(e4_match, code20, fam1.x, first.x, dob.x), code20) %>%
#   print(n = Inf)

# continue with 2024 sample that was not matched
e5 <- filter(e4, !pid4 %in% e4_match$pid4)

# continue with 2020 sample that was not matched
p5 <- filter(p4, !pid4 %in% e4_match$pid4)

nrow(e5) # 1,856 left to match by other means
nrow(p5) # 2,344 left to match by other means

# [5] pid = code, first (when unique to code) -----------------------------
#
# note: 2 first name(s) are missing and replaced with "NA", which makes this
# step very moderately risky

# n = 1,818 remaining voters have (a) unique first name(s) at the city-level
group_by(e5, code) %>%
  add_count(first, name = "n_first") %>%
  filter(n_first == 1)

e5 <- group_by(e5, code) %>%
  add_count(first, name = "n_first") %>%
  rowwise() %>%
  mutate(pid5 = rlang::hash(str_c(code, first)),
         pid5 = if_else(n_first == 1, pid5, NA_character_)) %>%
  select(-n_first) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(e5$pid5)))

p5 <- group_by(p5, code20) %>%
  add_count(first, name = "n_first") %>%
  rowwise() %>%
  mutate(pid5 = rlang::hash(str_c(code20, first)),
         pid5 = if_else(n_first == 1, pid5, NA_character_)) %>%
  select(-n_first) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(p5$pid5)))

# this time, we have to keep only those rows with a valid `pid5`
e5_match <- filter(e5, !is.na(pid5)) %>%
  full_join(filter(p5, !is.na(pid5)), by = "pid5") %>%
  filter(!is.na(code20), !is.na(code)) # keep only matches

nrow(e5_match) # 38 matches
# arrange(select(e5_match, code20, fam1.x, first.x, dob.x), code20) %>%
#   print(n = Inf)

# continue with 2024 sample that was not matched
e6 <- filter(e5, !pid5 %in% e5_match$pid5)

# continue with 2020 sample that was not matched
p6 <- filter(p5, !pid5 %in% e5_match$pid5)

nrow(e6) # 1,818 left to match by other means
nrow(p6) # 2,306 left to match by other means

# [6] pid = code, first (when unique to code) -----------------------------

# n = 1,808 remaining voters have a unique date of birth at the city-level
group_by(e6, code) %>%
  add_count(dob, name = "n_dob") %>%
  filter(n_dob == 1)

e6 <- group_by(e6, code) %>%
  add_count(dob, name = "n_dob") %>%
  rowwise() %>%
  mutate(pid6 = rlang::hash(str_c(code, dob)),
         pid6 = if_else(n_dob == 1, pid6, NA_character_)) %>%
  select(-n_dob) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(e6$pid6)))

p6 <- group_by(p6, code20) %>%
  add_count(dob, name = "n_dob") %>%
  rowwise() %>%
  mutate(pid6 = rlang::hash(str_c(code20, dob)),
         pid6 = if_else(n_dob == 1, pid6, NA_character_)) %>%
  select(-n_dob) %>%
  ungroup()

# no duplicated `pid`
stopifnot(!duplicated(na.omit(p6$pid6)))

# this time, we have to keep only those rows with a valid `pid6`
e6_match <- filter(e6, !is.na(pid6)) %>%
  full_join(filter(p6, !is.na(pid6)), by = "pid6") %>%
  filter(!is.na(code20), !is.na(code)) # keep only matches

nrow(e6_match) # 18 matches
# arrange(select(e6_match, code20, fam1.x, first.x, dob.x), code20) %>%
#   print(n = Inf)

# continue with 2024 sample that was not matched
e7 <- filter(e6, !pid6 %in% e6_match$pid6)

# continue with 2020 sample that was not matched
p7 <- filter(p6, !pid6 %in% e6_match$pid6)

nrow(e7) # 1,800 left to match by other means
nrow(p7) # 2,288 left to match by other means

# coda --------------------------------------------------------------------

# voters from 2024 identified in 2020: n = 8,564 (includes 1 duplicate)
nrow(e) - nrow(e7)
(nrow(e) - nrow(e7)) / nrow(e) # 82% of 2024 sample (based on EU elec. lists)

# voters from 2020 identified in 2024: n = 8,564
nrow(p) - nrow(p7)
(nrow(p) - nrow(p7)) / nrow(p) # 78% of 2020 sample (dataset)

# assemble panel
e_final <- bind_rows(
  # method 1: 10,853 matches (`pid` never missing at that stage)
  # note: 8,386 of these matches are found in `e` (i.e. in 2024)
  filter(e1, !is.na(code20)),
  # method 2: 98 matches (`pid` missing for unmatched observations from 2020)
  filter(e2, !is.na(code20), !is.na(pid)),
  # method 3: 11 matches (`pid` missing for unmatched observations from 2020)
  filter(e3, !is.na(code20), !is.na(pid)),
  # method 4: 13 matches
  select(e4_match, pid = pid.x, everything()),
  # method 5: 38 matches
  select(e5_match, pid = pid.x, pid4 = pid4.x, everything()),
  # method 6: 18 matches
  select(e6_match, pid = pid.x, pid4 = pid4.x, pid5 = pid5.x, everything())
) %>%
  # drop duplicated personal info + rename columns
  select(-ends_with(".y")) %>%
  rename_with(~ str_remove(.x, ".x$"))

# 8,564 voters from 2024 + 2,467 unmatched observations from 2020
table(e_final$pid %in% e$pid)

prop.table(table(e_final$pid %in% e$pid)) # matched 77% of 2024 sample
table(e_final$pid %in% e$pid) / nrow(p)   # matched 79% of 2020 sample (almost)

# single duplicate from 2024 is still here
e_final[ duplicated(e_final$pid), ]

# stopifnot(!duplicated(e_final$pid))

# final remarks -----------------------------------------------------------

# difference in sex ratio is less than 1 point
mean(p$female)
mean(e_final$female[ e_final$pid %in% e$pid ])

# largest difference in Round 1 voting behaviour (values 0-2) ~ 0.6 point
round(prop.table(table(p$t1)), 3)
round(prop.table(table(e_final$t1[ e_final$pid %in% e$pid ])), 3)

# largest difference in Round 2 voting behaviour (values 0-2) ~ 1 point
round(prop.table(table(p$t2)), 3)
round(prop.table(table(e_final$t2[ e_final$pid %in% e$pid ])), 3)

# small number of people changed cities between 2020 and 2024: n = 186
# (based only on matches produced by method 1; other steps miss city changes)
filter(filter(e_final, !is.na(code20)), code != code20)

# export ------------------------------------------------------------------

# n = 8,564
filter(e_final, !is.na(code)) %>%
  select(code, pid, mun_r1 = t1, mun_r2 = t2) %>%
  mutate_if(is.double, as.integer) %>%
  readr::write_tsv("data-lists/panel/panel.tsv")

# kthxbye
