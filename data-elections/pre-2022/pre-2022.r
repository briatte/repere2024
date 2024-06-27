# scrape presidential election 2022

library(rvest)
library(tidyverse)

fs::dir_create("html")
fs::dir_create("html/index")
fs::dir_create("html/cities")

b <- str_c("https://www.archives-resultats-elections.interieur.gouv.fr/",
           "resultats/presidentielle-2022/032/059/")

d <- tibble::tibble() # turnout
r <- tibble::tibble() # results (votes)
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

    u <- str_c(b, str_sub(j, 15))

    f <- str_c("html/cities/", str_extract(j, "59\\d{3}"), ".html")
    # cat(f)
    if (!fs::file_exists(f)) {

      download.file(u, f, mode = "wb", quiet = TRUE)

      # briefly wait a third of the time
      if (runif(1) < 1/3)
        Sys.sleep(1)

    }
    h <- read_html(f)

    # find turnout table(s) (two rounds, four tables)
    v <- html_nodes(h, "table.fr-table") %>%
      map(html_table)

    w <- map_lgl(v, ~ names(.x)[2] == "Nombre") %>%
      which()

    # sanity check: tables 2 and 4 are turnout tables, 1 and 3 are vote tables
    stopifnot(w == c(2, 4))

    # inscrits, abstentions, votants, blancs, nuls, exprimés
    d <- map(v[ w ], set_names, 1:4) %>%
      map(select, what = `1`, n = `2`) %>%
      # enable binding of '1 397' (chr) with '546' (int)
      map(mutate, n = as.character(n)) %>%
      bind_rows() %>%
      # city name and postal code
      tibble::add_column(city = html_text(html_nodes(h, "h3")[[2]])) %>%
      tibble::add_column(code = j) %>%
      bind_rows(d, .)

    # results
    r <- map(v[ -w ], set_names, 1:4) %>%
      map(select, candidate = `1`, n = `2`) %>%
      # enable binding of '1 397' (chr) with '546' (int)
      map(mutate, n = as.character(n)) %>%
      bind_rows() %>%
      # city name and postal code
      tibble::add_column(city = html_text(html_nodes(h, "h3")[[2]])) %>%
      tibble::add_column(code = j) %>%
      bind_rows(r, .)

  }

}

# finalize turnout --------------------------------------------------------

d <- d %>%
  mutate(what = str_sub(str_to_lower(what), 1, 3),
         n = as.integer(str_remove_all(n, "\\D")),
         city = str_sub(city, 12),
         code = as.integer(str_extract(code, "59\\d{3}"))) %>%
  group_by(city) %>%
  mutate(round = 1:n(),
         round = if_else(round < 7, 2L, 1L)) %>%
  tidyr::pivot_wider(id_cols = c(city, code, round),
                     names_from = what, values_from = n)

stopifnot(!is.na(d$round))

n_distinct(d$city[ d$round == 1 ]) # 648
stopifnot(sum(d$round == 1) == 648)
stopifnot(sum(d$round == 2) == 648)

# export turnout, round 1
filter(d, round == 1) %>%
  select(city, code, ins, abs) %>%
  readr::write_tsv("turnout-pre-2022-r1.tsv")

# export turnout, round 2
filter(d, round == 2) %>%
  select(city, code, ins, abs) %>%
  readr::write_tsv("turnout-pre-2022-r2.tsv")

# finalize results --------------------------------------------------------

r <- r %>%
  mutate(candidate = str_remove(candidate, "^M(me|\\.)\\s") %>%
           str_remove("^(\\w|-)+\\s") %>%
           str_to_title(),
         n = as.integer(str_remove_all(n, "\\D")),
         city = str_sub(city, 12),
         code = as.integer(str_extract(code, "59\\d{3}"))) %>%
  group_by(city) %>%
  mutate(round = 1:n(),
         round = if_else(round < 3, 2L, 1L))

# export results, round 1
filter(r, round == 1) %>%
  select(city, code, candidate, n) %>%
  readr::write_tsv("results-pre-2022-r1.tsv")

# export results, round 2
filter(r, round == 2) %>%
  select(city, code, candidate, n) %>%
  readr::write_tsv("results-pre-2022-r2.tsv")

# kthxbye
