library(readxl)
library(tidyverse)

u <- "https://www.insee.fr/fr/statistiques/fichier/6683031/dep59.xlsx"
f <- "data-cities/population/insee-6683031-dep59.xlsx"

if (!fs::file_exists(f)) {
  download.file(u, f, mode = "wb", quiet = TRUE)
}

d <- readxl::read_excel(f, sheet = 3, skip = 7) %>%
  select(code = `Code commune`, city = `Nom de la commune`,
         pop20 = `Population totale`) %>%
  mutate(code = str_c(59, code))

# at that stage, I just know the number of cities by heart
stopifnot(nrow(d) == 648)

# total population size = 2.6 million
sum(d$pop20) # 2,637,650

# mean ~ 4,000, median ~ 1,300, max ~ 238,000
summary(d$pop20)

readr::write_tsv(d, "data-cities/population/population-2020.tsv")

# kthxbye
