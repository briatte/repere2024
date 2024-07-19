# compare old and new prefectural lists, 14 May 2024 v. 9 July 2024
# requires zipped data, obviously

# initial filename: Listes arrêtées Nord 140524.zip
# production date: 14 May 2024
z_old <- "data-lists/electorate/Listes-59-2024-05-14-old.zip"

# initial filename: ListesArrêtées-dep59-09-07-2024-12h14-1731184.zip
# production date: 9 July 2024
z_new <- "data-lists/electorate/Listes-59-2024-07-09.zip"

# assemble ----------------------------------------------------------------

f <- unzip(z_old, list = TRUE) %>%
  pull(Name) %>%
  str_subset("csv$")

# all cities are there
stopifnot(length(f) == 648)

# aggregate (takes a minute or so)
d_old <- map_dfr(f, ~ unz(z_old, .x) %>%
                   readr::read_delim(delim = ";", col_types = cols(.default = "c"),
                                     show_col_types = FALSE, id = "code")) %>%
  mutate(code = str_extract(code, "59\\d{3}"))

f <- unzip(z_new, list = TRUE) %>%
  pull(Name) %>%
  str_subset("csv$")

# all cities are there
stopifnot(length(f) == 648)

# aggregate (takes a minute or so)
d_new <- map_dfr(f, ~ unz(z_new, .x) %>%
                   readr::read_delim(delim = ";", col_types = cols(.default = "c"),
                                     show_col_types = FALSE, id = "code")) %>%
  mutate(code = str_extract(code, "59\\d{3}"))

d_old <- d_old %>%
  select(list = `libellé du type de liste`,
         fam1 = `nom de naissance`,
         first = `prénoms`,
         female = sexe,
         dob = `date de naissance`,
         nat = `identifiant nationalité`)

d_new <- d_new %>%
  select(list = `libellé du type de liste`,
         fam1 = `nom de naissance`,
         first = `prénoms`,
         female = sexe,
         dob = `date de naissance`,
         nat = `identifiant nationalité`)

# compare -----------------------------------------------------------------

nrow(d_old) # 1,835,865
nrow(d_new) # 1,840,917

# main and complementary lists
count(d_old, list) # EU =  9,950
count(d_new, list) # EU = 10,364

cat(10364 - 9950, "new EU voters\n")

# hash --------------------------------------------------------------------

d_old <- filter(d_old, list == "Liste complémentaire européenne")
stopifnot(d_old$nat != "FR")

cat("Hashing", format(nrow(d_old), big.mark = ","), "rows in old file...\n")

# fast (hashing only EU voters)
d_old <- mutate(d_old, first = str_replace_na(first)) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup()

# sanity check: everyone got (uniquely) hashed
stopifnot(!is.na(d_old$pid))

d_new <- filter(d_new, list == "Liste complémentaire européenne")
stopifnot(d_new$nat != "FR")

cat("Hashing", format(nrow(d_new), big.mark = ","), "rows in new file...\n")

# fast (hashing only EU voters
d_new <- mutate(d_new, first = str_replace_na(first)) %>%
  rowwise() %>%
  mutate(pid = rlang::hash(str_c(fam1, first, dob))) %>%
  ungroup()

# sanity check: everyone got (uniquely) hashed
stopifnot(!is.na(d_new$pid))

cat("Differences based on hashes:\n")

cat(length(setdiff(d_old$pid, d_new$pid)), "in old but not in new\n") # 224
# 224/9950 ~ 2%

cat(length(setdiff(d_new$pid, d_old$pid)), "in new but not in old\n") # 637
# 637/10364 ~ 6%

# example 'new' voter
anti_join(d_new, d_old, by = "pid") %>%
  filter(str_detect(fam1, "VALVERDE"))

# kthxbye
