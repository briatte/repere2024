# find non-French voters elected as municipal council members, or living in a
# city with an elected non-French council member, from same nationality or not

library(tidyverse)

# electoral lists
d <- readr::read_rds("data-lists/electorate/electorate-mun.rds")

# municipal council members (RNE, 16 April 2024)
cm <- "data-lists/councillors/elus-municipaux-cm-2-.csv" %>%
  read_csv2(col_types = cols(.default = "c")) %>%
  select(
    dept = `Code du département`,
    fam1 = `Nom de l'élu`,
    first = `Prénom de l'élu`,
    female = `Code sexe`,
    dob = `Date de naissance`,
    csp = `Code de la catégorie socio-professionnelle`,
    csp_name = `Libellé de la catégorie socio-professionnelle`,
    mandate_start = `Date de début du mandat`,
    fn_name = `Libellé de la fonction`,
    fn_start = `Date de début de la fonction`,
    nat = `Code nationalité`
  ) %>%
  filter(dept == "59") %>%
  mutate(first = str_replace_na(first), female = as.integer(female == "F")) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup()

elus <- inner_join(d, select(cm, -dept, -fam1, -first), by = "pid")
nrow(elus) # 13 matches

# sanity checks: matches confirmed by sex and nationality
stopifnot(identical(elus$female.x, elus$female.y))
stopifnot(identical(elus$nat.x, elus$nat.y))

count(elus, csp, csp_name) # self-declared
count(elus, mandate_start) # 2020 or 2021
count(elus, fn_name) # always empty

# n = 329 voters reside in a city with a non-French councillor
# ~ 3% of all non-French voters
table(d$code %in% elus$code)

# select them, and code a match by city, but NOT by nationality
nat0 <- filter(select(d, pid, code), code %in% unique(elus$code)) %>%
  add_column(cm_nat0 = 1L)

# n = 242 voters match the city AND nationality of a non-French councillor
# ~ 2.2% of all non-French voters
nat1 <- select(elus, code, nat = nat.y) %>%
  inner_join(select(d, pid, code, nat), by = c("code", "nat")) %>%
  add_column(cm_nat1 = 1L)

# sanity check: all councillors are included in that last sample
stopifnot(elus$pid %in% nat1$pid)

# finalize
d <- full_join(nat0, select(nat1, -code), by = "pid") %>%
  mutate(cm_seat = as.integer(pid %in% elus$pid),
         cm_nat1 = replace_na(cm_nat1, 0L)) %>%
  select(-nat)

# last sanity checks
stopifnot(identical(nrow(d), nrow(nat0)))
stopifnot(identical(sum(d$cm_seat), nrow(elus)))

# last checks
sum(d$cm_nat0) # 329
sum(d$cm_nat1) # 242
sum(d$cm_seat) #  13

# export
readr::write_tsv(d, "data-lists/councillors/councillors.tsv")

# kthxbye
