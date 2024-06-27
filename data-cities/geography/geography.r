# 1. download GeoJSON data on all cities
# 2. collect geometries into {sf} objects
# 3. export {sf} objects as RDS files

library(jqr)
library(sf)
library(tidyverse)

# download JSON/GeoJSON ---------------------------------------------------

# https://geo.api.gouv.fr/decoupage-administratif/communes

fs::dir_create("json")

d <- readr::read_tsv("data-cities/population/population.tsv")
stopifnot(!duplicated(d$code))

b <- "https://geo.api.gouv.fr/"
for (i in rev(d$code)) {

  u <- str_c(b, "communes?code=", i, "&fields=code,nom,centre,contour,bbox")
  f <- str_c("data-cities/geography/json/", i, ".json")

  if (!fs::file_exists(f)) {

    readr::write_file(httr::content(httr::GET(u), "text"), f)

  # if you prefer {httr2}: https://httr2.r-lib.org/
  # r <- httr2::request(str_c(b, "communes?code=", i, "&fields=code,nom,centre"))
  # r <- httr2::req_perform(r)

  }

  if (!which(d$code == i) %% 25) {
    cat(d$city[ d$code == i ], which(d$code == i) - 1, "to go...\n")
  }

}

# parse JSON files --------------------------------------------------------

f <- fs::dir_ls("json", glob = "*json")

# city codes
n <- str_remove_all(f, "\\D")

# slow, but accepted by {jqr} parser (required since {sf} rejects the files)
j <- purrr::map(f, readr::read_lines)

# contours (polygons) -----------------------------------------------------

# slow, but works
geo <- j %>%
  purrr::map(jqr::jq, ".[] | .contour") %>%
  purrr::map(sf::st_read, quiet = TRUE) %>%
  bind_rows %>%
  # much faster than extracting from JSON sources with line below
  # purrr::map(j, jqr::jq, ".[] | .code")
  add_column(code = n)

readr::write_rds(geo, "data-cities/geography/city-contours.rds")

# centres (points) --------------------------------------------------------

ctr <- j %>%
  purrr::map(jqr::jq, ".[] | .centre") %>%
  purrr::map(sf::st_read, quiet = TRUE) %>%
  bind_rows() %>%
  add_column(code = n)

readr::write_rds(ctr, "data-cities/geography/city-centres.rds")

# kthxbye
