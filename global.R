library(dygraphs)
library(ggplot2)
library(leaflet)
library(spdplyr)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(HydroData)
library(AOI)

options(shiny.sanitize.errors = FALSE)

# Add additional R files
source("R/map_tab.R")
source("R/data_tab.R")
source("R/info_tab.R")
source("R/high_flows_tab.R")
source("R/filter_tab.R")

# Define base variables
month = as.numeric(substr(list.files("data/current_nc")[1], 1,2))
month_files = list.files("data/", pattern = as.character(month), full.names = T)

# Returns COMIDS from stream names
# getIDs("Monument Creek COMID: 1529685") -> "1529685"
# getIDs(c("Monument Creek COMID: 1529685", "Fountain Creek COMID: 1529677")) -> "1529685" "1529677"
getIDs <- function(streams) {
  purrr::map_chr(streams, ~unlist(strsplit(., split='COMID: ', fixed=TRUE))[2])
}

# Show or hide all provided elements
show_hide_all <- function(elements, action) {
  capture.output(purrr::map(elements, get(action)))
}


# Get location
get_location <- function(place) {
  # Check if input is likely a lat/lon pair
  split = unlist(strsplit(place, split=" ", fixed=TRUE))
  if ((length(split) == 2) && !is.na(as.numeric(split[1])) && !is.na(as.numeric(split[2])) )  {
    lat = as.numeric(split[1])
    lon = as.numeric(split[2])
  } 
  else {
    point = ifelse(place == "", "National Water Center", place)
    loc = AOI::geocode(location = point)
    lat = loc$lat
    lon = loc$lon
  }
  return(list(lat = lat, lon = lon))
}

# latlong2state(lat = loc$lat, lon = loc$lon)
latlong2state <- function(lat, lon) {
  
  df = data.frame(lon = lon, lat = lat)
  pt = sf::st_as_sf(df, coords = c('lon', 'lat'), crs = AOI::aoiProj)
  conus = getAOI(state = "conus") %>% AOI::bbox_st()
  # 
  # lat = dplyr::between(pointsDF$y, conus$ymin, conus$ymax)
  # lng = dplyr::between(pointsDF$x, conus$xmin, conus$xmax)
  
  if (AOI::is_inside(pt, conus)) {
  #if((lat+lng) != 2){ 
    return(NULL)
  } else {
    
    # tmp = sp::SpatialPoints(coords = cbind(pointsDF$x, pointsDF$y), 
    #                         proj4string = sp::CRS(AOI::aoiProj)) %>% sf::st_as_sf()
    t = AOI::counties[pt,]
    
    return(list(county = t$name,
                state  = t$state_name,
                state.abb = t$state_abbr))
    
  }
}