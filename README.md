# REPERE 2024

The code and data in this repo are part of the [REPERE][repere] (_Recherche Empirique sur la Participation Électorale des Ressortissants Européens_) and [ECREMEE][ecremee] research projects. The repo focuses on the _département du Nord_ and the EU elections of 2024.

[repere]: https://sms.univ-tlse2.fr/accueil-sms/la-recherche/operations-structurantes/repere-recherche-empirique-sur-la-participation-electorale-des-ressortissants-europeens
[ecremee]: https://anr.fr/Project-ANR-22-CE41-0006

See [Kelbel _et al._ 2024][pag24] for earlier results on the 2020 municipal elections.

[pag24]: https://doi.org/10.17645/pag.7507

## Contents

## `data-cities`

City-level data:

- `census` -- population characteristics extracted from the last census
- `geography` -- spatial boundaries and centroids (source: [Etalab, 2024][geo])
- `population` -- legal population sizes (source: [Insee, 2024][legal-pop])

[geo]: https://geo.api.gouv.fr/
[legal-pop]: https://www.insee.fr/fr/statistiques/7739582

## `data-elections`

Electoral data, scraped from the [Web archive][elections] of the Ministry of the Interior:

- `eur-2024` -- turnout in the 2024 EU elections
- `mun-2020` -- turnout in the 2020 municipal elections
- `pre-2024` -- turnout and results of the 2022 presidential election

[elections]: https://www.archives-resultats-elections.interieur.gouv.fr/

## `data-lists`

Data from the 2024 electoral lists:

- `coverage` -- data collection coverage (source: fieldwork)
- `electorate` -- registered voters (source: Préfecture du Nord, 2024)
  - variable `pid` contains 32-bit hashes based on family names, first names (replaced by "NA" if missing) and dates of birth
- `votes`
- `procurations`

__N.B.__ The scripts in this folder rely on restricted-access datasets.

## `plot-cartograms`

Contiguous and non-overlapping circle cartograms of abstention and turnout rates.

Example:

![](plot-cartograms/cartogram-pct_abs-eur24.jpg)

## `plot-choropleths`

Maps of electorate sizes in the 2024 EU elections.

Example:

![](plot-choropleths/choropleth-n_eur.jpg)

## `plot-turnout`

Scatterplots of turnout in the 2024 EU elections vs. previous years:

![](plot-turnout/plot-turnout.jpg)

## `plot-turnout`

Scatterplots of vote shares in the 2024 EU elections vs. the 2022 presidential election.

Example:

![](plot-vshares/plot-vshares-rn-ensemble.jpg)

## Dependencies

```r
library(tidyverse)
fs::dir_ls(glob = "*r", recurse = TRUE) %>% 
    map(read_lines) %>% 
    map(str_subset, "library\\(") %>% 
    unlist() %>% 
    unique() %>% 
    sort() %>% 
    cat(sep = "\n")
    
library(cartogram)
library(ggrepel)
library(jqr)
library(patchwork)
library(rvest) # bundled with the {tidyverse}
library(sf)
library(tidyverse)
```

## HOWTO

The code below will refresh all datasets, plots and reports.

```r
# fs::dir_ls(glob = "*r", recurse = TRUE)

# [1] generate datasets

# city-{centres,contours}.rds
source("data-cities/geography/geography.r")
# population.tsv
source("data-cities/population/population.r")
# turnout-eur-2024.tsv
source("data-elections/eur-2024/eur-2024.r")
# results-eur-2024.tsv
source("data-elections/eur-2024/results-eur-2024.r")
# turnout-mun-2020-r[12].tsv
source("data-elections/mun-2020/mun-2020.r")
# turnout-pre-2022-r[12], results-pre-2022-r[12].tsv
source("data-elections/pre-2022/pre-2022.r")
# vshares-pre-2022.tsv
source("data-elections/pre-2022/vshares-pre-2022.r")
# coverage.tsv
source("data-lists/coverage/coverage.r")
# electorate-{eur,mun,ppl}.rds, electorate-counts.tsv
source("data-lists/electorate/electorate.r")

# [2] generate plots

source("data-lists/coverage/plot-coverage.r")
source("plot-cartograms/plot-cartograms.r")
source("plot-choropleths/plot-choropleths.r")
source("plot-turnout/plot-turnout.r")
source("plot-vshares/plot-vshares.r")

# [3] generate reports
library(knitr)

# coverage.md
knitr::knit("data-lists/coverage.rmd")
```
