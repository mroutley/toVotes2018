library(dplyr)
# library(devtools)
# devtools::install_github("Psepho/toCensus")
# devtools::install_github("Psepho/toVotes")
library(toCensus)
library(toVotes)
data(toCensus)
data(toVotes)

vote_geo <- left_join(toVotes, toPollGeo)
votes_by_area <- toVotes %>%
  group_by(year, ward, area) %>%
  summarize(votes = sum(votes))
votes_by_area <- left_join(votes_by_area, toPollGeo)
library(ggmap)
library(mapproj)
toronto_map <- qmap("queens park, toronto")#, zoom = 11, maptype = 'terrain')
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=votes), alpha = 5/6, data=votes_by_area) +
  scale_colour_brewer("Votes") +
  facet_wrap(~year)
