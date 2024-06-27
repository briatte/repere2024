# extract EU election results, since that wasn't done during scraping
# nice opportunity to put `map` and `transmute` to good use

library(rvest)
library(tidyverse)

# assumes that the raw data have been zipped
z <- "data-elections/eur-2024/html.zip"
j <- z %>%
  unzip(list = TRUE) %>%
  filter(str_detect(Name, "cities/59.*html")) %>%
  pull(Name)

# slow-ish, but avoids fetching a file somewhere else
r_city <- map(j, ~ read_html(unz(z, .x)) %>%
                html_node("h4") %>%
                html_text())

# reading files again, sure, but it's fast enough
r_vote <- map(j, ~ read_html(unz(z, .x)) %>%
                html_nodes("table.fr-table") %>%
                pluck(1) %>%
                html_table(convert = FALSE))

# bind city/code + results, finalize and export
map2_dfr(r_city, r_vote, ~ bind_cols(city = .x, .y), .id = "code") %>%
  transmute(code = str_extract(city, "\\d{5}"),
            city = str_squish(str_remove(city, "\\(\\d{5}\\)")),
            list = `Liste des candidatures par suffrages exprimés`,
            nuance = Nuance,
            n = str_remove_all(Voix, "\\s") %>%
              str_replace(",", ".") %>%
              as.numeric()) %>%
  readr::write_tsv("data-elections/eur-2024/results-eur-2024.tsv")

# kthxbye
