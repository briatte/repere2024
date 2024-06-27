library(tidyverse)

u <- "https://www.insee.fr/fr/statistiques/fichier/7739582/ensemble.zip"
f <- fs::path_file(u)

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = TRUE)
}

d <- readr::read_csv2(unz(f, "donnees_communes.csv")) %>%
  filter(DEP == "59")

# at that stage, I just know the number of cities by heart
stopifnot(nrow(d) == 648)

# total population size = 2.6 million
sum(d$PTOT) # 2641207

# mean ~ 4,000, median ~ 1,300, max ~ 238,500
summary(d$PTOT)

select(d, code = COM, city = Commune, pop21 = PTOT) %>%
  readr::write_tsv("population.tsv")

# kthxbye
