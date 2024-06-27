library(patchwork)
library(tidyverse)

# vote shares for RN 2024 v. Le Pen 2022

pre22 <- readr::read_tsv("data-elections/pre-2022/vshares-pre-2022.tsv")
eur24 <- readr::read_tsv("data-elections/eur-2024/results-eur-2024.tsv") %>%
  group_by(city, code) %>%
  summarise(exp24 = sum(n),
            bardella = n[ nuance == "LRN" ] / exp24,
            ensemble = n[ nuance == "LENS" ] / exp24,
            melenchon = n[ nuance == "LFI" ] / exp24,
            glucksmann = n[ nuance == "LUG" ] / exp24) %>%
  ungroup()

lbl <- format(10^(1:5), big.mark = ",", scientific = FALSE, trim = TRUE)

v <- full_join(pre22, select(eur24, -city), by = "code") %>%
  # add electorate size
  full_join(readr::read_tsv("data-elections/eur-2024/turnout-eur-2024.tsv") %>%
              select(-city),
            by = "code")

# RN 2024 v 2022
p1 <- ggplot(v, aes(100 * le_pen_r1, 100 * bardella)) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins)) +
  ggrepel::geom_label_repel(data = filter(v, ins > 2e4),
                            aes(label = city),
                            box.padding = 0.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  labs(title = "Vote share for RN, 2024 v. 2022",
       subtitle = str_c("Pearson \u03c1 = ",
                        round(cor(v$le_pen_r1, v$bardella), 2)),
       y = "% vote for RN in 2024 (EU)",
       x = "% vote for RN in 2022 (presidential, first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

# Ensemble 2024 v 2022
p2 <- ggplot(v, aes(100 * macron_r1, 100 * ensemble)) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins)) +
  ggrepel::geom_label_repel(data = filter(v, ins > 2e4),
                            aes(label = city),
                            box.padding = 0.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  labs(title = "Vote share for Ensemble, 2024 v. 2022",
       subtitle = str_c("Pearson \u03c1 = ",
                        round(cor(v$macron_r1, v$ensemble), 2)),
       y = "% vote for Ensemble in 2024 (EU)",
       x = "% vote for Ensemble in 2022 (presidential, first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

# Mélenchon 2024 v 2022
p3 <- ggplot(v, aes(100 * melenchon_r1, 100 * melenchon)) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins)) +
  ggrepel::geom_label_repel(data = filter(v, ins > 2e4),
                            aes(label = city),
                            box.padding = 0.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  labs(title = "Vote share for LFI, 2024 v. 2022",
       subtitle = str_c("Pearson \u03c1 = ",
                        round(cor(v$melenchon_r1, v$melenchon), 2)),
       y = "% vote for LFI in 2024 (EU)",
       x = "% vote for LFI in 2022 (presidential, first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

# Glucksmann 2024 v Mélenchon 2022
p4 <- ggplot(v, aes(100 * melenchon_r1, 100 * glucksmann)) +
  geom_density_2d_filled(alpha = 0.5, show.legend = FALSE) +
  geom_point(aes(size = ins)) +
  ggrepel::geom_label_repel(data = filter(v, ins > 2e4),
                            aes(label = city),
                            box.padding = 0.7, max.overlaps = Inf,
                            min.segment.length = 0, seed = 202459) +
  scale_size_area("Electorate size (2024)", max_size = 8,
                  breaks = 10^(1:5), labels = lbl, guide = "none") +
  labs(title = "Vote share for Glucksmann 2024 v. Mélenchon 2022",
       subtitle = str_c("Pearson \u03c1 = ",
                        round(cor(v$melenchon_r1, v$glucksmann), 2)),
       y = "% vote for Glucksmann in 2024 (EU)",
       x = "% vote for LFI in 2022 (presidential, first round)") +
  theme_linedraw() +
  theme(plot.title = element_text(face = "bold", hjust = 0))

p1 + p2 + plot_layout(ncol = 2, widths = c(1, 1), heights = NA) +
  plot_annotation(
    caption = str_c("Point areas are proportional to electorate size\n",
                    "Labels show cities with 20,000+ registered voters\n",
                    "Note: both x-axes and y-axes have different ranges")
  )

ggsave("plot-vshares/plot-vshares-rn-ensemble.jpg", width = 14, height = 7)

p3 + p4 + plot_layout(ncol = 2, widths = c(1, 1), heights = NA) +
  plot_annotation(
    caption = str_c("Point areas are proportional to 2024 electorate size\n",
                    "Labels show cities with 20,000+ registered voters\n",
                    "Note: both x-axes and y-axes have different ranges")
  )

ggsave("plot-vshares/plot-vshares-lfi-glucksmann.jpg", width = 14, height = 7)

# kthxbye
