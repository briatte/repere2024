# co-nationals: voters who share a family name
#

d <- read_rds("data-lists/electorate/electorate.rds") %>%
  filter(nat != "FR", list == "complémentaire européenne")

"COROIU"

# filter(d, `identifiant nationalité` != "FR") %>%
d %>%
  group_by(city, code) %>%
  add_count(fam1, name = "n_fam1") %>%
  add_count(fam2, name = "n_fam2") %>%
  filter(n_fam1 > 2 | (n_fam2 > 2 & !is.na(fam2))) %>%
  select(city, fam1, n_fam1, fam2, n_fam2, first, sex, dob) %>%
  arrange(city, fam1, fam2) %>%
  view()

view(d)
