library(dplyr)
library(leaflet)

#dir.create(paste0(getwd(),"/Flowlines"))
usgsStation = load('data/usgsStations.Rdata')
df = (5/2)/69

usgsIcon = makeIcon(
  iconUrl= "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
  iconWidth = 40, iconHeight = 20,
  iconAnchorX = 20, iconAnchorY = 10)
