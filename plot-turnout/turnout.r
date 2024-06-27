library(tidyverse)

# first rounds only
d <- full_join(
  readr::read_tsv("turnout-eur-2024/turnout-eur-2024.tsv") %>%
    mutate(turnout20 = 100 * (1 - abs / ins)) %>%
    select(city, code, ins20 = ins, turnout20),
  readr::read_tsv("turnout-mun-2020/turnout-mun-2020-r1.tsv") %>%
    mutate(turnout24 = 100 * (1 - abs / ins)) %>%
    select(code, ins24 = ins, turnout24),
  by = "code"
) %>%
  mutate(ins5000 = if_else(ins24 >= 5000, "> 5,000 voters", "< 5,000 voters"))

stopifnot(!is.na(d$turnout20))
stopifnot(!is.na(d$turnout24))

ggplot(d, aes(turnout20, turnout24, size = ins24)) +
  geom_point() +
  ggrepel::geom_label_repel(data = filter(d, ins24 > 50000),
                            aes(label = city)) +
  coord_equal() +
  scale_size_area(max_size = 8, guide = "none") +
  facet_wrap(~ ins5000) +
  xlim(c(20, 80)) +
  ylim(c(20, 80)) +
  labs(y = "% turnout in 2024 (European)",
       x = "% turnout in 2020 (municipal)") +
  theme_linedraw()

ggsave("turnout-2024-vs-2020.png", width = 7, height = 7)
