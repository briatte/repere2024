# requires electoral lists zipped into `Listes arrêtées Nord 140524.zip`
# file used comes from the préfecture du Nord and was provided to David Gouard
# on May 15, 2024; more up-to-date lists will follow

library(tidyverse)

z <- "data-cities/electorate/Listes arrêtées Nord 140524.zip"
f <- unzip(z, list = TRUE) %>%
  pull(Name) %>%
  str_subset("csv$")

# all cities are there
stopifnot(length(f) == 648)

# takes a minute or so
d <- map_dfr(f, ~ unz(z, .x) %>%
               readr::read_delim(delim = ";", col_types = cols(.default = "c"),
                                 show_col_types = FALSE, id = "code")) %>%
  mutate(code = str_extract(code, "59\\d{3}"))

# total electorate size ~ 1.83 million
nrow(d)

d <- d %>%
  select(code,
                city = `libellé de l'ugle`,
                list = `libellé du type de liste`,
                sex = sexe,
                dob = `date de naissance`,
                nat = `identifiant nationalité`) %>%
  mutate(bdy = as.Date(str_replace(dob, "\\d{4}$", "2024"), "%d/%m/%Y"),
         age = 2023 - as.integer(str_extract(dob, "\\d{4}")),
         # exact age at election date (works only on valid dates, but 972 voters
         # have '00' as day or month of birth, a lot of them born during WW2, so
         # sticking with year of birth for them; affects < 0.1% of the sample)
         age = if_else(is.na(bdy), age, age + (bdy < as.Date("2024-06-09"))),
         age6 = cut(age, c(18, 25, 35, 45, 55, 65, Inf), right = FALSE),
         sex = as.integer(sex == "F"),
         list = str_remove(list, "Liste ")) %>%
  select(-bdy)

# sanity check: codes and city names overlap perfectly
group_by(d, code) %>%
  summarise(n_city = n_distinct(city)) %>%
  filter(n_city > 1)

group_by(d, city) %>%
  summarise(n_code = n_distinct(code)) %>%
  filter(n_code > 1)

# main and compl. lists
count(d, list)

# sanity check: only nationals on main lists
stopifnot(d$nat[ d$list == "principale" ] == "FR")
# sanity check: only non-nationals on compl. lists
stopifnot(d$nat[ d$list == "complémentaire municipale" ] != "FR")
stopifnot(d$nat[ d$list == "complémentaire européenne" ] != "FR")

# top 5: BE, PT, IT, ES, DE
print(count(d, nat, sort = TRUE), n = Inf)

# relabel age groups
ages <- as.integer(str_sub(levels(d$age6), 2, 3))
ages <- str_c(ages, "-", lead(ages) - 1)
levels(d$age6) <- c(na.omit(ages), "65+")

# basic demographics
count(d, sex)
count(d, age6, sort = TRUE) # top group = 65+, bottom group = 18-24

stopifnot(!is.na(d$sex))
stopifnot(!is.na(d$age))
stopifnot(!is.na(d$age6))

# export temp. data (not to be uploaded)
readr::write_rds(d, "data-cities/electorate/electorate.rds", compress = "gz")

# non-French counts, by nationality
nat <- d %>%
  filter(nat != "FR") %>%
  mutate(nat = str_c("n_", str_to_lower(nat))) %>%
  group_by(code, city) %>%
  count(nat) %>%
  pivot_wider(names_from = "nat", values_from = "n") %>%
  ungroup()

# order columns by decreasing counts
nat <- nat[, c(1:2, 2 + order(colSums(nat[, -(1:2) ]), decreasing = TRUE)) ]

# total count + French and non-French counts
d %>%
  group_by(code, city) %>%
  summarise(
    n_ins = n(),
    n_nat = sum(nat == "FR"),
    n_eur = sum(nat != "FR")
  ) %>%
  ungroup() %>%
  # append nationality columns
  left_join(select(nat, -city), by = "code") %>%
  # replace NA with 0s
  mutate_if(is.integer, replace_na, 0) %>%
  readr::write_tsv("data-cities/electorate/electorate-counts.tsv")

# kthxbye
