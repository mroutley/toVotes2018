library(magrittr)

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
if(file.exists("data-raw/voting_location_2014_wgs84.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_location_2014_wgs84.zip",
                destfile = "data-raw/voting_location_2014_wgs84.zip")
  unzip("data-raw/voting_location_2014_wgs84.zip", exdir="data-raw/voting_location_2014_wgs84")
}
toronto_wards <- sf::st_read("data-raw/voting_location_2014_wgs84", layer = "VOTING_LOCATION_2014_WGS84") %>%
  sf::st_transform(crs = "+init=epsg:4326")

# Position scores ---------------------------------------------------------

positions <-
  readxl::read_excel("data-raw/Natural positions.xlsx") %>%
  tidyr::gather(issue, value, -CT) %>%
  dplyr::mutate(CT = as.character(CT))


# Maps --------------------------------------------------------------------

to_positions <- census_tracts %>%
  sf::st_intersection(toronto_wards) %>%
  dplyr::left_join(positions, by = c("CTUID" = "CT")) %>%
  dplyr::select(CTUID, issue, value) %>%
  sf::st_as_sf()

ggplot() + geom_sf(data = subset(to_positions, issue = "Issue 1"), aes(fill = value))

ggplot() + geom_sf(data = to_positions)
