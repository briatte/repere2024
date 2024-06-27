library(patchwork)
library(tidyverse)

# first rounds only
d <- full_join(
  readr::read_tsv("data-elections/eur-2024/turnout-eur-2024.tsv") %>%
    mutate(trn24 = 100 * (1 - abs / ins)) %>%
    select(city, code, ins24 = ins, trn24),
  readr::read_tsv("data-elections/pre-2022/turnout-pre-2022-r1.tsv") %>%
    mutate(trn22 = 100 * (1 - abs / ins)) %>%
    select(code, ins22 = ins, trn22),
  by = "code"
) %>%
  full_join(
    readr::read_tsv("data-elections/mun-2020/turnout-mun-2020-r1.tsv") %>%
      mutate(trn20 = 100 * (1 - abs / ins)) %>%
      select(code, ins20 = ins, trn20),
    by = "code"
  ) %>%
  mutate(ins5000 = if_else(ins24 >= 5000, "> 5,000", "< 5,000"))

stopifnot(!is.na(d$trn20))
stopifnot(!is.na(d$trn22))
stopifnot(!is.na(d$trn24))

lbl <- format(10^(1:5), big.mark = ",", scientific = FALSE, trim = TRUE)

p1 <- ggplot(d, aes(trn20, trn24)) +
  # geom_bin_2d(alpha = 0.5, show.legend = FALSE) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins24), alpha = 0.75) +
  scale_fill_viridis_d(option = "C") +
  ggrepel::geom_label_repel(data = filter(d, ins24 > 2.5e4),
                            aes(label = city),
                            box.padding = 1.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  # coord_equal() +
  # scale_color_manual("", values = c("> 5,000" = "black", "< 5,000" = "grey50")) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  xlim(10 * range(d$trn20 %/% 10)) +
  ylim(10 * range(d$trn24 %/% 10)) +
  labs(title = "Turnout in 2024 (EU elections) v. 2020 (municipal elections)",
       subtitle = str_c("Pearson \u03c1 = ", round(cor(d$trn20, d$trn24), 2)),
       y = "% turnout in 2024",
       x = "% turnout in 2020 (first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

p2 <- ggplot(d, aes(trn22, trn24)) +
  # geom_bin_2d(alpha = 0.5, show.legend = FALSE) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins24), alpha = 0.75) +
  scale_fill_viridis_d(option = "C") +
  ggrepel::geom_label_repel(data = filter(d, ins24 > 2.5e4),
                            aes(label = city),
                            box.padding = 1.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  # coord_equal() +
  # scale_color_manual("", values = c("> 5,000" = "black", "< 5,000" = "grey50")) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  xlim(10 * range(d$trn22 %/% 10)) +
  ylim(10 * range(d$trn24 %/% 10)) +
  labs(title = "Turnout in 2024 (EU elections) v. 2022 (presidential election)",
       subtitle = str_c("Pearson \u03c1 = ", round(cor(d$trn22, d$trn24), 2)),
       y = "% turnout in 2024",
       x = "% turnout in 2022 (first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1), heights = NA) +
  plot_annotation(caption = "Note: x-axes have different ranges")

ggsave("plot-turnout/plot-turnout.jpg", width = 14, height = 7)
