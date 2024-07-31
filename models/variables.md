# `analyse-inscription-59.do`

Level = voters (`1`), nested in nationality (`N`) + city (`c`) dyads

| Registration   |   | Level | Description | Notes |
|:---------------|---|:-:|:------------|:------|
|`t1_2` (DV)     |   | 1 | 1 = registered | . |
|`nat2` (cluster)| × | N/c | nationality + city dyad | . |
|`age60`         | × | c | % aged 60+ | . |
|`assoc_eur`     |   | . | national association | __CODING?__ |
|`assoc_nat1`    |   | . | national association | __CODING?__ |
|`bac2`          | × | c | % bac+2 | . |
|`candnat`       | – | – | co-national candidate | _not applicable to 2024_ |
|`comp`          | – | – | competitiveness (\# lists) | _not applicable to 2024_ |
|`dens`          |   | . | density of nationality | __REQUIRES CENSUS DATA__ |
|`dens`          |   | . | density of nationality | __REQUIRES CENSUS DATA__ |
|`empou`         | × | c | % employés/ouvriers | __CHECK CODING__ |
|`femme`         | × | c | % females (city-level) | __CHECK CODING__ |
|`front[23]`     | × | c | border status | . |
|`lepen`         | × | c | % Le Pen PR 2017 | update to % Le Pen PR 2022; __include Zemmour?__ |
|`orient`        | × | c | city-level pol. majority | __subsample, n = 138 cities__ |
|`part_m2020`    | × | c | city-level turnout | __UPDATE to EU 2024 or PR 2022__ |
|`res20`         |   | c | % residents for 20+ years | __SOURCE?__ |
|`tc`            | × | c | city size | copy categories |
|`tc3`           | × | c | city size | copy categories |

# `analyse-vote-59.do`

Level = registered EU voters (`1`), nested in nationality (`N`) + city (`c`) dyads

| Participation       |   | Level | Description | Notes |
|:--------------------|---|:-:|:------------|:------|
|`tinscrit2` (DV)     |   | 1 | 1 = voted | . |
|`codec` (cluster)    | × | c | city (Insee code) | . |
|`nat` (cluster)      | × | N | nationality | . |
|`nat2` (cluster)     | × | N/c | nationality + city dyad | . |
|`age`                | × | 1 | age | . |
|`agesq`              | × | 1 | age^2 | . |
|`assoc_nat1`         |   | . | national association | __CODING?__ |
|`belge`              | × | 1 | is Belgian | . |
|`candnat`            | – | – | co-national candidate | _not applicable to 2024_ |
|`comp`               | – | – | competitiveness (\# lists) | _not applicable to 2024_ |
|`dens`               |   | . | density of nationality | __REQUIRES CENSUS DATA__ |
|`front[2345]`        | × | c | border status | __dataset has only first two__ |
|`lepen`              | × | c | % Le Pen PR 2017 | update to % Le Pen PR 2022; __include Zemmour?__ |
|`logpop`             | × | c | log(city population) | . |
|`naissf`             | × | 1 | born in France | . |
|`orient`             | × | c | city-level pol. majority | __subsample, n = 138 cities__ |
|`orient2`            | × | c | city-level pol. majority | __subsample, n = 138 cities__ |
|`part_m2020`         | × | c | city-level turnout | __UPDATE to EU 2024 or PR 2022__ |
|`sexe`               | × | 1 | sex | . |
|`tc`                 | × | c | city size | copy categories |
|`tc2`                | × | c | city size | copy categories |
