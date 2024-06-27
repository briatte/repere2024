# scrape European election 2024 results for the Nord (59)

library(rvest)
library(tidyverse)

fs::dir_create("html")
fs::dir_create("html/index")
fs::dir_create("html/cities")

b <- str_c("https://www.archives-resultats-elections.interieur.gouv.fr/",
           "resultats/europeennes2024/ensemble_geographique/32/59/")

d <- tibble::tibble()
for (i in LETTERS) {

  u <- str_c(b, i, ".php")

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
    html_nodes(xpath = "//a[contains(@href, './59')]") %>%
    html_attr("href")

  cat(":", length(h), "cities...\n")

  for (j in h) {

    u <- str_c(b, str_sub(j, 3))

    f <- str_c("html/cities/", str_extract(j, "59\\d{3}"), ".html")
    if (!fs::file_exists(f)) {

      download.file(u, f, mode = "wb", quiet = TRUE)

      # briefly wait a third of the time
      if (runif(1) < 1/3)
        Sys.sleep(1)

    }
    h <- read_html(f)

    # inscrits, abstentions, votants, blancs, nuls, exprimés
    v <- html_nodes(h, "table.fr-table") %>%
      pluck(2) %>%
      html_table() %>%
      select(X1, X2) %>%
      filter(X1 != "") %>%
      # city name and postal code
      tibble::add_column(city = html_text(html_node(h, "h4"), trim = TRUE))

    d <- bind_rows(d, v)

  }

}

# finalize turnout --------------------------------------------------------

d %>%
  mutate(X1 = str_sub(str_to_lower(X1), 1, 3),
         X2 = as.integer(str_remove_all(X2, "\\D"))) %>%
  pivot_wider(id_cols = city, names_from = X1, values_from = X2) %>%
  separate(city, sep = "\\(", into = c("city", "code")) %>%
  mutate(city = str_squish(city),
         code = as.integer(str_sub(code, end = -2))) %>%
  # turnout = 100 * (1 - abs / ins)
  select(city, code, ins, abs) %>%
  readr::write_tsv("turnout-eur-2024.tsv")

# kthxbye
