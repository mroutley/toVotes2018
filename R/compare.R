library(magrittr)
library(toVotes)

# Aggregate to census tracts ----------------------------------------------

# Shapefiles -------------------------------------------------------------------

library(rgeos)
library(rgdal)

# Census tracts
if(file.exists("data-raw/gct_000b11a_e.zip")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
                destfile = "data-raw/gct_000b11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
}
census_tracts <- rgdal::readOGR(dsn = "data-raw/gct", layer = "gct_000b11a_e") %>%
  sp::spTransform(sp::CRS('+init=epsg:4326'))

# Toronto wards
if(file.exists("data-raw/voting_location_2014_wgs84.zip")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_location_2014_wgs84.zip",
                destfile = "data-raw/voting_location_2014_wgs84.zip")
  unzip("data-raw/voting_location_2014_wgs84.zip", exdir="data-raw/voting_location_2014_wgs84")
}
toronto_wards <- rgdal::readOGR(dsn = "data-raw/voting_location_2014_wgs84", layer = "VOTING_LOCATION_2014_WGS84") %>%
  sp::spTransform(sp::CRS('+init=epsg:4326'))
# Subset the CTs to just those in Toronto
to_census_tracts <- census_tracts[toronto_wards,]
toronto_wards@data %<>%
  dplyr::mutate(ward = as.integer(stringr::str_sub(PT_LNG_CD, 1, 2)), # Extract Ward ID
                area = as.integer(stringr::str_sub(PT_LNG_CD, -3, -1))) # Extract Area ID

# Spatial aggregation -----------------------------------------------------

# We need a column for each candidate for the aggregation

# Subset to just major candidates

major_candidates <- toVotes %>% # Restrict major candidates to anyone that recieved more than 10,000 votes
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

# Join votes to shapefiles, by district and poll numbers

to_geo_votes <- toronto_wards
to_geo_votes@data <- dplyr::select(to_geo_votes@data, ward, area)
to_geo_votes@data <- dplyr::left_join(to_geo_votes@data, spread_votes)

# Consider merging these into a single Spatial object
to_ct_votes <- aggregate(x = to_geo_votes, by = to_census_tracts, FUN = sum, na.rm = TRUE, areaWeighted = TRUE)
to_ct_votes$ct_id <- to_census_tracts$CTUID
to_ct_votes_data <- to_ct_votes@data %>%
  tidyr::gather(candidate, votes, -ward, -area, -ct_id)

actuals <- to_ct_votes_data %>%
  dplyr::select(CT = ct_id,
                candidate,
                votes)
actuals$candidate[actuals$candidate == "Keesmaat.Jennifer"] <- "Keesmaat"
actuals$candidate[actuals$candidate == "Tory.John"] <- "Tory"

actuals %<>% tibble::as_tibble() %>%
  tidyr::spread(candidate, votes) %>%
  dplyr::mutate(total = Keesmaat + Tory) %>%
  dplyr::mutate(Keesmaat = Keesmaat/total,
                Tory = Tory/total) %>%
  dplyr::select(-total) %>%
  tidyr::gather(candidate, actual_proportion, -CT)

predictions <- readr::read_csv("data-raw/predictions.csv")

compare <- dplyr::left_join(actuals, predictions) %>%
  dplyr::mutate(difference = actual_proportion - predicted_proportion) %>%
  dplyr::rename(ct_id = CT) %>%
  dplyr::filter(candidate == "Keesmaat",
                !is.na(difference)) %>%
  dplyr::select(ct_id, difference)

to_ct_votes@data %<>%
  dplyr::left_join(compare)

ct_geo_compare <- ggplot2::fortify(to_ct_votes, region = "ct_id") %>%
  dplyr::left_join(compare, by = c("id" = "ct_id"))

library(ggmap)
library(mapproj)
ggmap::register_google(key = getOption("GMAP_API"))
toronto_map <- qmap("queens park,toronto", zoom = 11, maptype = 'terrain')
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(difference, 5)), alpha = 5/6, data = ct_geo_compare) +
  scale_fill_brewer("Difference", palette = "YlOrBr", labels=c("Low", "", "", "", "High"))
