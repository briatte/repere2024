# scrape municipal election 2020 results for the Nord (59)

library(rvest)
library(tidyverse)

fs::dir_create("html")
fs::dir_create("html/index")
fs::dir_create("html/cities")

b <- str_c("https://www.archives-resultats-elections.interieur.gouv.fr/",
           "resultats/municipales-2020/059/")

d <- tibble::tibble()
for (i in LETTERS) {

  u <- str_c(b, "059", i, ".php")

  f <- fs::path("html/index", fs::path_file(u))
  cat(f)
  if (!fs::file_exists(f)) {

    h <- try(download.file(u, f, mode = "wb", quiet = TRUE), silent = TRUE)
    if ("try-error" %in% class(h)) {
      cat(f, ": empty, skipping\n")
      next
    }

  }

  h <- read_html(f) %>%
    html_nodes(xpath = "//a[contains(@href, '/059/059')]") %>%
    html_attr("href") %>%
    str_subset("059\\d{3}")

  cat(":", length(h), "cities...\n")

  for (j in h) {

    u <- str_c(b, str_sub(j, 8))

    f <- str_c("html/cities/", str_extract(j, "59\\d{3}"), ".html")
    if (!fs::file_exists(f)) {

      download.file(u, f, mode = "wb", quiet = TRUE)

      # briefly wait a third of the time
      if (runif(1) < 1/3)
        Sys.sleep(1)

    }
    h <- read_html(f)

    # find turnout table(s) (one or two if two rounds)
    v <- html_nodes(h, "table.fr-table") %>%
      map(html_table)

    w <- map_lgl(v, ~ names(.x)[2] == "Nombre") %>%
      which()

    # sanity check: one or two tables max
    stopifnot(length(w) %in% 1:2)

    # inscrits, abstentions, votants, blancs, nuls, exprimés
    v <- map(v[w], set_names, 1:4) %>%
      map(select, what = `1`, n = `2`) %>%
      # enable binding of '1 397' (chr) with '546' (int)
      map(mutate, n = as.character(n)) %>%
      bind_rows() %>%
      # city name and postal code
      tibble::add_column(city = html_text(html_nodes(h, "h3")[[2]])) %>%
      tibble::add_column(code = j)

    # sanity check: each table has 6 rows
    stopifnot(nrow(v) %in% c(6, 12))

    d <- bind_rows(d, v)

  }

}

# finalize turnout --------------------------------------------------------

d <- d %>%
  mutate(what = str_sub(str_to_lower(what), 1, 3),
         n = as.integer(str_remove_all(n, "\\D")),
         city = str_sub(city, 19),
         code = as.integer(str_extract(code, "59\\d{3}"))) %>%
  group_by(city) %>%
  # if 12 rows, 2 rounds: first 6 rows = round 2, next 6 are round 1
  # if 6 rows, single round: all rows = round 1
  mutate(round = 1:n(),
         round = case_when(
           n() == 6 ~ 1,
           n() == 12 & round < 7 ~ 2,
           n() == 12 & round > 6 ~ 1,
           .default = NA)) %>%
  pivot_wider(id_cols = c(city, code, round),
              names_from = what, values_from = n)

stopifnot(!is.na(d$round))

n_distinct(d$city[ d$round == 1 ]) # 648
n_distinct(d$city[ d$round == 2 ]) # 92

# export round 1
filter(d, round == 1) %>%
  select(city, code, ins, abs) %>%
  readr::write_tsv("turnout-mun-2020-r1.tsv")

# export round 2
filter(d, round == 2) %>%
  select(city, code, ins, abs) %>%
  readr::write_tsv("turnout-mun-2020-r2.tsv")

# kthxbye
