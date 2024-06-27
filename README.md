# REPERE 2024

![](plot/cartograms/cartogram-pct_abs-eur24.jpg)

The code and data in this repo are part of the [REPERE][repere] (_Recherche Empirique sur la Participation Électorale des Ressortissants Européens_) and [ECREMEE][ecremee] research projects. The repo focuses on the _département du Nord_ and the EU elections of 2024.

[repere]: https://sms.univ-tlse2.fr/accueil-sms/la-recherche/operations-structurantes/repere-recherche-empirique-sur-la-participation-electorale-des-ressortissants-europeens
[ecremee]: https://anr.fr/Project-ANR-22-CE41-0006

See [Kelbel _et al._ 2024][pag24] for earlier results on the 2020 municipal elections.

[pag24]: https://doi.org/10.17645/pag.7507

## Contents

## `data-scrapers`

Scrapers for electoral data:

- `eur-2024` -- turnout in the 2024 EU elections
- `mun-2020` -- turnout in the 2020 municipal elections
- `pre-2024` -- turnout and results of the 2022 presidential election

## `data-lists`

Data from the 2024 electoral lists:

- `votes`
- `procurations`

## `data-cities`

City-level data:

- `electorate` -- registered voters 
  (source: Préfecture du Nord, 2024)
- `population` -- legal population sizes 
  (source: [Insee, 2024][legal-pop])
- `sample` -- cities included and excluded of past (and future) models

[legal-pop]: https://www.insee.fr/fr/statistiques/7739582
