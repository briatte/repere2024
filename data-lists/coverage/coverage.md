## Weight of missing cities in overall city sample

|                     | Cities | % cities |    Voters | % voters | EU voters | % EU voters |
|:--------------------|-------:|---------:|----------:|---------:|----------:|------------:|
| Observed            |    428 |     66.0 | 1,631,641 |     89.4 |     8,978 |        90.2 |
| Observed but empty  |    152 |     23.5 |   115,875 |      6.3 |       356 |         3.6 |
| Unobserved, missing |     68 |     10.5 |    77,376 |      4.2 |       616 |         6.2 |
| Total               |    648 |    100.0 | 1,824,892 |    100.0 |     9,950 |       100.0 |

Sources: fieldwork notes, prefectural lists.

“Observed but empty” means that the cities were inspected during
fieldwork but did not provide a complementary list of EU voters.

Remarks:

- The voting lists of 68 cities (10% of total) could not be observed
  during fieldwork, as they could not be located by the prefectural
  staff.
- In addition, 152 cities did not separate EU voters from French
  nationals in their lists, which led us to drop them from the
  observation pool.
- Roughly 10% of EU voters are registered in these missing cities, which
  are all small-size cities with less than 5,000 residents.
- The amount of missing data is very reasonable, and is unlikely to
  cause sampling bias.

## Spatial distribution of missing cities

![](coverage-choropleth.jpg)

Given the limited amount of missing data, we did not test for complete
spatial randomness, but a rough test of median latitude and longitude of
the city centroids did not return any significant effect.

## Consistency with 2020 model samples

The models are those featured in [Kelbel *et al.*
2024](https://doi.org/10.17645/pag.7507), which focused on the 2020
municipal elections.

### Consistency with **Model 1** (restricted to cities \> 5,000 residents)

| status              | Included | Ignored | Excluded |
|:--------------------|---------:|--------:|---------:|
| Observed            |       80 |      59 |      289 |
| Observed but empty  |        1 |      91 |       60 |
| Unobserved, missing |        0 |      12 |       56 |

“Ignored” means that the data contained no EU voters for these cities.

Single problematic case (included in 2020 but unobserved or empty in
2024): Dechy (8 registered EU voters).

Kelbel *et al.* 2024 report a sample size of 6,268 voters in the 101
cities included in Model 1. Using prefectural lists and legal
populations of 2021, our estimate is that the same model estimated on
the 2024 sample will likely include **6,593** voters from **106**
cities.

### Consistency with **Model 4** (estimated on full city sample)

| status              | Included | Ignored | Excluded |
|:--------------------|---------:|--------:|---------:|
| Observed            |      364 |      59 |        5 |
| Observed but empty  |       61 |      91 |        0 |
| Unobserved, missing |       56 |      12 |        0 |

Number of problematic cases (included in 2020 but unobserved or empty in
2024): 117 cities, totalling 907 registered EU voters (9.1% of all
registered EU voters).

Top 5 problematic cases: Cousolre (88 EU voters), Taisnières-sur-Hon (80
EU voters), Ghyvelde (58 EU voters), Gognies-Chaussée (54 EU voters),
Eppe-Sauvage (30 EU voters).

Kelbel *et al.* 2024 report a sample size of 9,827 voters in the 486
cities included in Model 4. Our estimate is that the same model
estimated on the 2024 sample will likely include **8,978** voters from
**428** cities.
