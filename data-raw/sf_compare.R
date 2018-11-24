library(magrittr)
library(toVotes)
predictions <- readr::read_csv("data-raw/predictions.csv") %>%
  dplyr::filter(CT != "Grand Total") %>%
  dplyr::rename(ct_id = CT)

# Shapefiles --------------------------------------------------------------

# Census tracts
if(file.exists("data-raw/gct_000b11a_e.zip")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
                destfile = "data-raw/gct_000b11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
}
census_tracts <- sf::st_read("data-raw/gct", layer = "gct_000b11a_e") %>%
  sf::st_transform(crs = "+init=epsg:4326")

# Toronto wards
if(file.exists("data-raw/voting_location_2018_wgs84.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_location_2018_wgs84.zip",
                destfile = "data-raw/voting_location_2018_wgs84.zip")
  unzip("data-raw/voting_location_2018_wgs84.zip", exdir="data-raw/voting_location_2018_wgs84")
}
toronto_wards <- sf::st_read("data-raw/voting_location_2018_wgs84", layer = "VOTING_LOCATION_2018_WGS84") %>%
  sf::st_transform(crs = "+init=epsg:4326")

# Subset the CTs to just those in Toronto
ct_to <- census_tracts[toronto_wards,]

# Actual results -----------------------------------------------------

# Subset to just major candidates

major_candidates <- toVotes %>% # Restrict major candidates to anyone that recieved more than 100,000 votes
  dplyr::filter(year == 2018) %>%
  dplyr::group_by(candidate) %>%
  dplyr::summarise(votes = sum(votes)) %>%
  dplyr::filter(votes > 100000) %>%
  dplyr::arrange(desc(votes)) %>%
  dplyr::select(candidate)

spread_votes <- toVotes %>%
  dplyr::filter(year == 2018) %>%
  dplyr::right_join(major_candidates) %>%
  dplyr::select(-type, -year) %>%
  dplyr::filter(!is.na(area)) %>%
  tidyr::spread(candidate, votes)

to_geo_votes <- toronto_wards %>%
  dplyr::mutate(ward = as.integer(stringr::str_sub(PT_LNG_CD, 1, 2)), # Extract Ward ID
                area = as.integer(stringr::str_sub(PT_LNG_CD, -3, -1))) %>% # Extract Area ID
  dplyr::left_join(spread_votes) %>%
  dplyr::select(`Keesmaat Jennifer`, `Tory John`) %>%
  dplyr::rename(Keesmaat = `Keesmaat Jennifer`, Tory = `Tory John`)

actuals <- aggregate(x = to_geo_votes, by = ct_to, FUN = sum) %>%
  dplyr::mutate(ct_id = ct_to$CTUID)
actuals %<>%
  tibble::as_tibble() %>%
  dplyr::select(-geometry) %>%
  dplyr::mutate(total = Keesmaat + Tory) %>%
  dplyr::mutate(Keesmaat = Keesmaat/total,
                Tory = Tory/total) %>%
  dplyr::select(-total) %>%
  tidyr::gather(candidate, actual_proportion, -ct_id)

compare <- dplyr::left_join(actuals, predictions) %>%
  dplyr::mutate(difference = actual_proportion - predicted_proportion) %>%
  dplyr::filter(candidate == "Keesmaat") %>%
  dplyr::select(ct_id, difference)

geo_compare <- ct_to %>%
  dplyr::left_join(compare, by = c("CTUID" = "ct_id")) %>%
  dplyr::select(difference)
plot(geo_compare)
