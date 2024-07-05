# requires electoral lists zipped into `Listes arrêtées Nord 140524.zip`

library(tidyverse)

z <- "data-lists/electorate/Listes arrêtées Nord 140524.zip"
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
stopifnot(min(d$age) == 18) # minimum voting age
stopifnot(!is.na(d$age6))

# checks on variables used for hashing
stopifnot(!is.na(d$fam1))
stopifnot(!is.na(d$dob))
sum(is.na(d$first)) # n = 50 rows (48 voters) with no first name(s)

# invalid dates
sum(str_detect(d$dob, "00/")) # n = 972 (< 0.1% of total)

# main and complementary lists
count(d, list)

# sanity check: only nationals on main lists
stopifnot(d$nat[ d$list == "principale" ] == "FR")

# sanity check: only non-nationals on complementary lists
stopifnot(d$nat[ d$list == "complémentaire européenne" ] != "FR")
stopifnot(d$nat[ d$list == "complémentaire municipale" ] != "FR")

# hashing -----------------------------------------------------------------

cat("Hashing", format(nrow(d), big.mark = ","), "rows...\n")
d <- mutate(d, first = str_replace_na(first)) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup()

# sanity check: everyone got (uniquely) hashed
stopifnot(!is.na(d$pid))

# export problematic cases: n = 2 voters on all three lists
group_by(d, pid) %>%
  mutate(n_lists = n_distinct(list)) %>%
  filter(n_lists > 2) %>%
  arrange(pid, fam1, first, dob) %>%
  write_tsv("data-lists/electorate/electorate-problematic-cases.tsv")

# exports -----------------------------------------------------------------

# total electorate size ~ 1.82 million
cat("Exporting", format(n_distinct(d$pid), big.mark = ","), "voters...\n")

# export municipal election list (n = 10,973)
filter(d, list == "complémentaire municipale") %>%
  readr::write_rds("data-lists/electorate/electorate-mun.rds", compress = "gz")

# export EU elections list (n = 9,950)
filter(d, list == "complémentaire européenne") %>%
  readr::write_rds("data-lists/electorate/electorate-eur.rds", compress = "gz")

# export main (French) list (n = 1,814,942)
filter(d, list == "principale") %>%
  readr::write_rds("data-lists/electorate/electorate-ppl.rds", compress = "gz")

# counts ------------------------------------------------------------------

# non-French counts, by nationality
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
