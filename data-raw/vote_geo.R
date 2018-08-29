library(magrittr)
library(tidyverse)

# Download shapefiles -----------------------------------------------------
years <- c(2006, 2010, 2014)
base_shapefile_url <- "http://opendata.toronto.ca/gcc/"
shapefile_prefix <- "data-raw/subdivision_"
for (year in years) {
  if(file.exists(paste(shapefile_prefix, year,".zip",sep=""))) {
    # Nothing to do
  }  else {
    download.file(paste0(base_shapefile_url, ifelse(year == 2014, "voting_subdivsion_", "voting_subdivision_"), year, "_wgs84.zip"),
                  destfile = paste0(shapefile_prefix,year,".zip"))
    unzip(paste(shapefile_prefix, year, ".zip", sep=""), exdir="data-raw")
  }
  # shape <- paste("shapefile_", year, sep="")
  # file <- paste("voting_subdivision",year,"_wgs84",sep="")
  # assign(shape,rgdal::readOGR(dsn = "data-raw", layer = file))
  # rm(file,shape)
}
rm(year,years)

prov_geo <- sf::st_read("data-raw/prov_shapefile", layer = "PDs_Ontario") %>%
  sf::st_transform(crs = "+init=epsg:4326")
rm(base_shapefile_url, pd_shapefile, prov_shapefile)

# Shapefiles --------------------------------------------------------------
# Extract the data object for each year and then combine into one data frame
geo_2014 <- ggplot2::fortify(shapefile_2014,region="AREA_NAME")
geo_2014$year <- as.integer(2014)
geo_2010 <- ggplot2::fortify(shapefile_2010,region="AREA_NAME")
geo_2010$year <- as.integer(2010)
geo_2006 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME")
geo_2006$year <- as.integer(2006)
geo_2003 <- ggplot2::fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 geo
geo_2003$year <- as.integer(2003)


