library(FlowFinder)
library(dygraphs)
options(shiny.sanitize.errors = FALSE)

# Add additional R files
source("map_tab.R")
source("data_tab.R")
source("info_tab.R")
source("high_flows_tab.R")
source("filter_tab.R")

# Define base variables
month = as.numeric(substr(list.files("data/current_nc")[1], 1,2))
month_files = list.files("data/", pattern = as.character(month), full.names = T)
norm = fst::read_fst(path = month_files)
size = 15

positive_pal <- colorBin(c('red', 'blue'), bins = c(-1, 0.000000000000000001, 9000000000000000000000))

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

#latlong2state(lat = loc$lat, lon = loc$lon)
latlong2state <- function(lat, lon) {
  
  pointsDF = list(x=lon, y = lat)
  conus = AOI::states[!(AOI::states$state_abbr %in% c("HI", "AK", 'PR')),]
  conus = AOI::getBoundingBox(conus)
  lat = dplyr::between(pointsDF$y, conus@bbox[2,1], conus@bbox[2,2])
  lng = dplyr::between(pointsDF$x, conus@bbox[1,1], conus@bbox[1,2])
  
  if((lat+lng) != 2){
    return(NULL)
  } 
  
  else {
    
    tmp = sp::SpatialPoints(coords = cbind(pointsDF$x, pointsDF$y), proj4string = sp::CRS(AOI::aoiProj))
    tmp = sp::spTransform(tmp, AOI::counties@proj4string)
    t = AOI::counties[tmp,]
    
    return(list(county = t$name,
                state  = t$state_name,
                state.abb = t$state_abbr))
    
  }
}