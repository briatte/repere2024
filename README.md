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
- `electorate` -- registered voters (source: Préfecture du Nord, 2024)
- `population` -- legal population sizes (source: [Insee, 2024][legal-pop])
- `sample` -- cities included and excluded of past (and future) models

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

- `votes`
- `procurations`

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
