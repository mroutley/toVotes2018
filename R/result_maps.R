library(magrittr)

# Shapefiles --------------------------------------------------------------

# # Census tracts
# if(file.exists("data-raw/gct_000b11a_e.zip")) {
#   # Nothing to do
# }  else {
#   download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip",
#                 destfile = "data-raw/gct_000b11a_e.zip")
#   unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw/gct")
# }
# census_tracts <- sf::st_read("data-raw/gct", layer = "gct_000b11a_e") %>%
#   sf::st_transform(crs = "+init=epsg:4326")
#
# # Toronto wards
# if(file.exists("data-raw/voting_location_2014_wgs84.zip")) {
#   # Nothing to do
# }  else {
#   download.file("http://opendata.toronto.ca/gcc/voting_location_2014_wgs84.zip",
#                 destfile = "data-raw/voting_location_2014_wgs84.zip")
#   unzip("data-raw/voting_location_2014_wgs84.zip", exdir="data-raw/voting_location_2014_wgs84")
# }
# toronto_wards <- sf::st_read("data-raw/voting_location_2014_wgs84", layer = "VOTING_LOCATION_2014_WGS84") %>%
#   sf::st_transform(crs = "+init=epsg:4326")

# Census tracts -----------------------------------------------------------
if(file.exists("data-raw/gct_000b11a_e.shp")) {
  # Nothing to do
}  else {
  download.file("http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gct_000b11a_e.zip", destfile = "data-raw/gct_000a11a_e.zip")
  unzip("data-raw/gct_000b11a_e.zip", exdir="data-raw")
}
proj4string=sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=GRS80 +towgs84=0,0,0")
ct_geo <- rgdal::readOGR(dsn = "data-raw", layer = "gct_000b11a_e")
# Toronto wards -----------------------------------------------------------
if(file.exists("data-raw/VOTING_SUBDIVISION_2010_WGS84.shp")) {
  # Nothing to do
}  else {
  download.file("http://opendata.toronto.ca/gcc/voting_subdivision_2010_wgs84.zip",
                destfile = "data-raw/subdivisions_2010.zip")
  unzip("data-raw/subdivisions_2010.zip", exdir="data-raw")
}
to_geo <- rgdal::readOGR(dsn = "data-raw", layer = "VOTING_SUBDIVISION_2010_WGS84")
# Harmonize the projections
ct_geo <- sp::spTransform(ct_geo, CRSobj = sp::CRS(sp::proj4string(to_geo)))
# Subset the CTs to just those in Toronto
ct_geo_to <- ct_geo[to_geo,]
ct_geo_to <- ggplot2::fortify(ct_geo_to, region = "CTUID")
# ct_geo_to <- transform(ct_geo_to, id = as.numeric(id))

# Position scores ---------------------------------------------------------

positions <-
  readxl::read_excel("data-raw/Natural positions.xlsx") %>%
  tidyr::gather(issue, value, -CT) %>%
  dplyr::mutate(CT = as.character(CT))

to_positions <- ct_geo_to %>%
  dplyr::left_join(positions, by = c("id" = "CT"))

# Predictions -------------------------------------------------------------

predictions <-
  readxl::read_excel("data-raw/2018 mayoral prediction.xlsx", sheet = "Summary") %>%
  dplyr::mutate(CT = `Row Labels`) %>%
  dplyr::select(CT, Tory, Keesmaat) %>%
  tidyr::gather(candidate, proportion, -CT) %>%
  dplyr::mutate(CT = as.character(CT))

to_predictions <- ct_geo_to %>%
  dplyr::left_join(predictions, by = c("id" = "CT"))

# Maps --------------------------------------------------------------------

library(ggmap)
library(mapproj)
toronto_map <- qmap("queens park,toronto", zoom = 11, maptype = 'terrain')
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(value, 5)), alpha = 5/6, data = subset(to_positions, !is.na(issue))) +
  scale_fill_brewer("Natural position", palette = "YlOrBr", labels=c("Left", "", "", "", "Right")) +
  facet_wrap(~issue)

# toronto_map +
#   geom_polygon(aes(x=long, y=lat, group=group, fill = cut_interval(value, 5)), alpha = 5/6, data = subset(to_positions, !is.na(issue))) +
#   scale_fill_brewer("Natural position", type = "div", labels=c("Left", "", "", "", "Right")) +
#   facet_wrap(~issue)

toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill = cut_interval(proportion, 5)), alpha = 5/6, data = subset(to_predictions, !is.na(proportion))) +
  scale_fill_brewer("Proportion of votes", palette = "YlOrBr", labels=c("Low", "", "", "", "High")) +
  facet_wrap(~candidate)
