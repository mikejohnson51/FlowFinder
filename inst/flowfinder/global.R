library(FlowFinder)
library(dygraphs)
options(shiny.sanitize.errors = FALSE)

# Add additional R files
source("map_tab.R")
source("data_tab.R")
source("info_tab.R")
source("high_flows_tab.R")

# Define base variables
month = as.numeric(substr(list.files("data/current_nc")[1], 1,2))
month_files = list.files("data/", pattern = as.character(month), full.names = T)
norm = fst::read_fst(path = month_files)
size = 15

# Returns COMIDS from stream names
# getIDs("Monument Creek COMID: 1529685") -> "1529685"
# getIDs(c("Monument Creek COMID: 1529685", "Fountain Creek COMID: 1529677")) -> "1529685" "1529677"
getIDs <- function(streams) {
  ids = c()
  for (stream in streams) {
    ids = c(ids, unlist(strsplit(stream, split='COMID: ', fixed=TRUE))[2])
  }
  return(ids)
}

# Show or hide all provided elements
show_hide_all <- function(elements, action) {
  if (action == "hide") {
    for (element in elements) {
      shinyjs::hide(element)
    }
  } else if (action == "show") {
      for (element in elements) {
        shinyjs::show(element)
      }
  } else if (action == "disable") {
      for (element in elements) {
        shinyjs::disable(element)
      }
  } else if (action == "enable") {
      for (element in elements) {
        shinyjs::enable(element)
      }
  }
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
    if (place == "") {
      point = "National Water Center"
    } 
    else {
      point = place
    }
    loc = AOI::getPoint(name = point)
    lat = loc$lat
    lon = loc$lon
  }
  return(list(lat = lat, lon = lon))
}


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
    
    tmp = sp::SpatialPoints(coords = cbind(pointsDF$x, pointsDF$y), proj4string = AOI::aoiProj)
    t = AOI::counties[tmp,]
    
    return(list(county = t$name,
                state  = t$state_name,
                state.abb = t$state_abbr))
    
  }
}



