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






