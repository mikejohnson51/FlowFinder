library(FlowFinder)
library(dygraphs)
options(shiny.sanitize.errors = FALSE)

# Add additional R files
source("map_tab.R")
source("data_tab.R")
source("info_tab.R")
source("high_flows_tab.R")
source("filter_tab.R")
print(getwd())
# Define base variables
month_string <- substr(list.files("data/current_nc")[1], 1,2)
month <- as.numeric(month_string)
# month = as.numeric(substr(list.files("data/current_nc")[1], 1,2))
month_files = list.files("data/", pattern = month_string, full.names = T)

# print(month_files)
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
latlong2state <- function(lat = 30.332184, lon = -81.655647) {
  
  
  
  pointsDF = data.frame(name = "tmp", x=lon, y = lat)
  states = AOI::aoi_get(state = "conus", county = "all")
  t <- states[sf::st_as_sf(pointsDF, coords = c("x", "y"), crs = sf::st_crs(states)),]
  
  if(nrow(t) == 0){
    return(NULL)
  } 
  
  else {
    
    return(list(county = t$name,
                state  = t$state_name,
                state.abb = t$state_abbr))
    
  }
}
