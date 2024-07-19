# process, separate and count national and non-national electorates
# requires electoral lists zipped into `Listes arrêtées Nord 140524.zip`

library(tidyverse)

z <- "data-lists/electorate/Listes-59-2024-07-09.zip"
f <- unzip(z, list = TRUE) %>%
  pull(Name) %>%
  str_subset("csv$")

# all cities are there
stopifnot(length(f) == 648)

# aggregate (takes a minute or so)
d <- map_dfr(f, ~ unz(z, .x) %>%
               readr::read_delim(delim = ";", col_types = cols(.default = "c"),
                                 show_col_types = FALSE, id = "code")) %>%
  mutate(code = str_extract(code, "59\\d{3}"))

# rename and recode -------------------------------------------------------

d <- d %>%
  select(code,
         city = `libellé de l'ugle`,
         list = `libellé du type de liste`,
         fam1 = `nom de naissance`,
         fam2 = `nom d'usage`,
         first = `prénoms`,
         female = sexe,
         dob = `date de naissance`,
         nat = `identifiant nationalité`) %>%
  mutate(bdy = as.Date(str_replace(dob, "\\d{4}$", "2024"), "%d/%m/%Y"),
         age = 2023 - as.integer(str_extract(dob, "\\d{4}")),
         # exact age at EU election date (works only on valid dates; see below)
         age = if_else(is.na(bdy), age, age + (bdy < as.Date("2024-06-09"))),
         age6 = cut(age, c(18, 25, 35, 45, 55, 65, Inf), right = FALSE),
         female = as.integer(female == "F"),
         list = str_remove(list, "Liste ")) %>%
  select(-bdy)

# relabel age groups
ages <- as.integer(str_sub(levels(d$age6), 2, 3))
ages <- str_c(ages, "-", lead(ages) - 1)
levels(d$age6) <- c(na.omit(ages), "65+")

# basic demographics
count(d, female)
count(d, age6, sort = TRUE) # top group = 65+, bottom group = 18-24

# 26 nationalities; top 5: BE, PT, IT, ES, DE
# print(count(d, nat, sort = TRUE), n = Inf)

# sanity checks -----------------------------------------------------------

# sanity check: codes and city names overlap perfectly
stopifnot(aggregate(code ~ city, d, n_distinct)$code == 1)
stopifnot(aggregate(city ~ code, d, n_distinct)$city == 1)

# sanity check: age and sex are never missing
stopifnot(!is.na(d$female))
stopifnot(!is.na(d$age))

# n = 2,482 (French) registered voters are actually less than 18 at the time
# of the 2024 EU election, which is... alright, I guess?
with(d, table(list, age < 18))

stopifnot(min(d$age[ d$nat != "FR" ]) == 18) # minimum voting age

# `age6` is only missing for (some French) voters below 18 years-old
stopifnot(!is.na(d$age6[ d$nat != "FR" ]))

# checks on variables used for hashing
stopifnot(!is.na(d$fam1))
stopifnot(!is.na(d$dob))
sum(is.na(d$first)) # n = 49 rows (48 voters?) with no first name(s)

# invalid dates
sum(str_detect(d$dob, "00/")) # n = 961 (< 0.1% of total)

# main and complementary lists
count(d, list)
# complémentaire européenne   10364
# complémentaire municipale   11175
# principale                1819378

# sanity check: only nationals on main lists
stopifnot(d$nat[ d$list == "principale" ] == "FR")

# sanity check: only non-nationals on complementary lists
stopifnot(d$nat[ d$list == "complémentaire européenne" ] != "FR")
stopifnot(d$nat[ d$list == "complémentaire municipale" ] != "FR")

# hashing -----------------------------------------------------------------

# hash 1,840,917 voters based on family name, first name (NA if missing) and
# date of birth; allows invalid dates of birth, changes of nationality, city
# or sex, as well as address and so on

cat("Hashing", format(nrow(d), big.mark = ","), "rows...\n")

# slow (takes a minute or so)
d <- mutate(d, first = str_replace_na(first)) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup()

# sanity check: everyone got (uniquely) hashed
stopifnot(!is.na(d$pid))

# first and third blocks below are both slow (a minute or so each)

# export problematic cases [1]: n = 1 voter show up on all 3 lists
group_by(d, pid) %>%
  filter(n_distinct(list) > 2) %>%
  arrange(pid, fam1, first, dob) %>%
  write_tsv("data-lists/electorate/electorate-problematic-cases-1.tsv")

# export problematic cases [2]: n = 2 EU voters show up in 2+ cities
filter(d, list != "principale") %>%
  group_by(pid) %>%
  filter(n_distinct(city) > 1) %>%
  arrange(pid, city, list, fam1, first, dob) %>%
  write_tsv("data-lists/electorate/electorate-problematic-cases-2.tsv")

# export problematic cases [3]: n = 353 French voters show up in 2+ cities
filter(d, list == "principale") %>%
  group_by(pid) %>%
  filter(n_distinct(city) > 1) %>%
  arrange(pid, city, list, fam1, first, dob) %>%
  write_tsv("data-lists/electorate/electorate-problematic-cases-3.tsv")

# registration of non-French voters ---------------------------------------

p <- filter(d, list != "principale") %>%
  group_by(pid) %>%
  summarise(
    ins_mun = as.integer("complémentaire municipale" %in% list),
    ins_eur = as.integer("complémentaire européenne" %in% list),
    ins_both = as.integer((ins_mun + ins_eur) == 2)
  )

sum(p$ins_mun)  # 11,173 -- two cases less, because of 2 duplicates
sum(p$ins_eur)  # 10,362 -- two cases less, because of 2 duplicates
sum(p$ins_both) #  9,868 -- 95% of those registered for EU elections

# exports -----------------------------------------------------------------

# main and complementary lists (recall)
count(d, list)

# total electorate size ~ 1.84 million
cat("Exporting", format(n_distinct(d$pid), big.mark = ","), "voters...\n")

# export municipal election list (n = 11,175)
filter(d, list == "complémentaire municipale") %>%
  # add registration dummies
  left_join(p, by = "pid") %>%
  readr::write_rds("data-lists/electorate/electorate-mun.rds", compress = "gz")

# export EU elections list (n = 10,364)
filter(d, list == "complémentaire européenne") %>%
  # add registration dummies
  left_join(p, by = "pid") %>%
  readr::write_rds("data-lists/electorate/electorate-eur.rds", compress = "gz")

# export main (French) list (n = 1,819,378)
filter(d, list == "principale") %>%
  readr::write_rds("data-lists/electorate/electorate-ppl.rds", compress = "gz")

# counts ------------------------------------------------------------------

# non-French counts (EU elections only), by nationality
nat <- filter(d, list == "complémentaire européenne") %>%
  filter(nat != "FR") %>%
  mutate(nat = str_c("n_", str_to_lower(nat))) %>%
  group_by(code, city) %>%
  count(nat) %>%
  # NAs have to be set to 0 for the column ordering right below to work
  pivot_wider(names_from = "nat", values_from = "n", values_fill = 0L) %>%
  ungroup()

# order columns by decreasing counts
nat <- nat[, c(1:2, 2 + order(colSums(nat[, -(1:2) ]), decreasing = TRUE)) ]

# total count + French and non-French counts
filter(d, list %in% c("principale", "complémentaire européenne")) %>%
  group_by(code, city) %>%
  summarise(
    n_ins = n(),
    n_nat = sum(nat == "FR"),
    n_eur = sum(nat != "FR")
  ) %>%
  ungroup() %>%
  # append nationality columns
  left_join(select(nat, -city), by = "code") %>%
  # replace NA with 0s on rows that were not in `nat` object
  mutate_if(is.integer, replace_na, 0) %>%
  readr::write_tsv("data-lists/electorate/electorate-counts.tsv")

# kthxbye
