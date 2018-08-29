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
    unzip(paste(shapefile_prefix, year, ".zip", sep=""), exdir=paste0("data-raw/", year))
  }

  assign(paste("geo", year, sep = "-"), sf::st_read(paste0("data-raw/", year)))
}
rm(year, years, base_shapefile_url, shapefile_prefix)

