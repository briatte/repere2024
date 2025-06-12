library(tidyverse)
library(banR) # remotes::install_github("joelgombin/banR")

# load JOAF data for dept. 59 (November 2024) -----------------------------

jo <- "data-associations/joaf-2024-12-09.csv" %>%
  readr::read_csv2(col_types = cols(.default = "c")) %>%
  select(id, association_type, typeAvis, etatAvis, datedeclaration, numero_rna,
         starts_with("titre"), objet, domaine_activite_libelle_categorise,
         adresse_actuelle, codepostal_actuel, commune_actuelle) %>%
  filter(association_type == "assoLoi1901") %>%
  # subset to before last EU election
  filter(as.Date(datedeclaration) < as.Date("2024-06-10"))

cat(nrow(jo), "rows...\n")

# match national keywords -------------------------------------------------

kw <- readr::read_csv("data-associations/keywords.csv")

ma <- select(jo, id, titre_search, objet) %>%
  add_column(matched = 0)

for (i in 1:nrow(kw)) {

  cat(kw$natl[ i ], "... ")

  r <- regex(kw$keywords[ i ], ignore_case = TRUE)
  r <- as.integer(str_detect(jo$titre_search, r) | str_detect(jo$objet, r))
  cat(sum(r == 1, na.rm = TRUE), "matches\n")

  ma$matched[ r == 1 ] <- 1
  # r <- tibble(found = if_else(r == 1, kw$natl[ i ], NA_character_),
  #             found_ok = if_else(is.na(found), NA_character_, "?")) %>%
  #   rename_with(~ str_replace(.x, "found", kw$iso3c[ i ]))

  r <- tibble(found = if_else(r == 1, kw$natl[ i ], NA_character_)) %>%
    rename_with(~ str_replace(.x, "found", kw$iso3c[ i ]))

  ma <- bind_cols(ma, r)

}

assocs <- filter(jo, id %in% ma$id[ ma$matched == 1 ])
cat(nrow(assocs), "rows matched at least once\n")

# find missing postal codes -----------------------------------------------

no_code <- filter(assocs, is.na(codepostal_actuel)) %>%
  select(id, adresse_actuelle)

cat("Geocoding", nrow(no_code), "addresses (using address)...\n")

# 1. using full address
no_code <- banR::geocode_tbl(no_code, adresse = adresse_actuelle)

assocs <- select(no_code, id, result_postcode, result_city) %>%
  right_join(assocs, by = "id") %>%
  mutate(
    codepostal_actuel = if_else(is.na(codepostal_actuel), result_postcode,
                                codepostal_actuel),
    commune_actuelle = if_else(is.na(commune_actuelle), result_city,
                               commune_actuelle)
  ) %>%
  select(-result_postcode, -result_city)

no_code <- filter(assocs, is.na(codepostal_actuel)) %>%
  select(id, adresse_actuelle) %>%
  mutate(adresse_actuelle = str_replace(adresse_actuelle, "(.*),(.*)", "\\2"))

cat("Geocoding", nrow(no_code), "addresses (using city)...\n")

# 2. using city name (or something close enough)
no_code <- banR::geocode_tbl(no_code, adresse = adresse_actuelle)

assocs <- select(no_code, id, result_postcode, result_city) %>%
  right_join(assocs, by = "id") %>%
  mutate(
    codepostal_actuel = if_else(is.na(codepostal_actuel), result_postcode,
                                codepostal_actuel),
    commune_actuelle = if_else(is.na(commune_actuelle), result_city,
                               commune_actuelle)
  ) %>%
  select(-result_postcode, -result_city)

stopifnot(!is.na(assocs$codepostal_actuel))

# get Insee city codes ----------------------------------------------------

cat("Getting Insee city codes...\n")

cities <- select(assocs, postcode = codepostal_actuel, commune_actuelle) %>%
  mutate(city = str_remove(commune_actuelle, "\\s[Cc][Eeé][Dd][Ee][Xx]") %>%
           str_remove_all("\\d+|,\\s|\\..*") %>%
           str_to_lower() %>%
           str_squish()) %>%
  distinct() %>%
  arrange(city) %>%
  banR::geocode_tbl(adresse = city, code_postal = postcode)

# minor disagreements
filter(cities, postcode != result_postcode) %>%
  arrange(commune_actuelle) %>%
  select(commune_actuelle, postcode, result_postcode, result_citycode)

# add to selected associations
assocs <- distinct(cities, commune_actuelle, postcode, result_citycode) %>%
  left_join(assocs, ., by = c("codepostal_actuel" = "postcode",
                            "commune_actuelle"))

stopifnot(!is.na(assocs$codepostal_actuel))

cat("Fixing a few more cases...\n")
still_no_code <- filter(assocs, is.na(result_citycode)) %>%
  select(id, commune_actuelle) %>%
  banR::geocode_tbl(adresse = commune_actuelle) %>%
  select(id, result_citycode2 = result_citycode)

assocs <- select(still_no_code, id, result_citycode2) %>%
  right_join(assocs, by = "id") %>%
  mutate(result_citycode = if_else(is.na(result_citycode), result_citycode2,
                                   result_citycode)) %>%
  select(-result_citycode2)

# manual fixes for extra-resistant cases
filter(assocs, is.na(result_citycode)) %>%
  select(id, commune_actuelle)

assocs$result_citycode[ str_detect(assocs$commune_actuelle,
                                   "59607 Maubeuge") ] <- "59392"
assocs$result_citycode[ str_detect(assocs$commune_actuelle,
                                   "59057 Roubaix") ] <- "59512"
assocs$result_citycode[ assocs$commune_actuelle ==
                          "SAINTAMANDLESEAUX" ] <- "59526"

stopifnot(!is.na(assocs$codepostal_actuel))

# export ------------------------------------------------------------------

# TODO: (done after export) handle 164 dissolved
assocs <- arrange(assocs, titre_search, datedeclaration) %>%
  group_by(titre_search) %>%
  mutate(dissolved = as.integer(last(typeAvis) == "Dissolution")) %>%
  ungroup()

filter(ma, matched == 1) %>%
  left_join(select(assocs, -titre_search, -objet), by = "id") %>%
  arrange(result_citycode) %>%
  readr::write_csv("data-associations/JOAF-2025-05-22.csv", na = "")

# TODO: (done after export) check cities with non-59xxx Insee codes
# TODO: (partially done since export) check keyword match relevance

# work in progress
