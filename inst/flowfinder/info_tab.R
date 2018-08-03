#Table 1: Station info
station_table <- function(values) {
  if(typeof(values$flow_data$nwis) == "S4") {
    station_data = cbind(paste0('<a href=',sprintf(
                            "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$flow_data$nwis$site_no),' target="_blank">',values$flow_data$nwis$site_name,"</a>"),
                         values$flow_data$nwis$site_no, 
                         round(values$flow_data$nwis$da_sqkm, digits = 0)
                         )
  } else {
    station_data = cbind('NA', 'NA', 'NA')
  }
  colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
  return(station_data)
}

# Table 2: Flowline info 
flowlines_table <- function(values) {
  
  max_order = ifelse(values$any_flow, max(values$flow_data$nhd@data$streamorde), "NA")
  stream_name = ifelse(values$any_flow, values$flow_data$nhd@data$gnis_name[match(max_order, values$flow_data$nhd@data$streamorde)], "NA")
  num_flowlines = ifelse(values$any_flow, length(values$flow_data$nhd), 0)
  num_wb = ifelse(exists('waterbodies', where=values$flow_data), length(values$flow_data$waterbodies), 0)
  size = 15^2
  unique_huc8 = ifelse(values$any_flow, paste(unique(as.numeric(na.omit(unique(substr(values$flow_data$nhd$reachcode,1,8))))), collapse = ", "), "NA")
  
  table = rbind(cbind("Largest Stream Order: ", max_order),
                cbind("Largest Stream Name: ", stream_name),
                cbind("Number of Flowlines: ", num_flowlines),
                cbind("Number of Water Bodies: ", num_wb),
                cbind("Total Area (SqMi): ", size),
                cbind("Unique HUC8 units: ", unique_huc8))
  colnames(table) = c('Hydrography', 'Value')
  return(table)
}

# Table 3: NWM info
nwm_table <- function(values) {
    dateTime = read.csv('data/current_nc/dateTime.csv', stringsAsFactors = FALSE)
    timeZone = lutz::tz_lookup_coords(values$loc$lat, values$loc$lon, method = "accurate")
    dif = as.POSIXct(paste(dateTime$date, dateTime$time), "%Y-%m-%d %H", tz = "GMT") - as.POSIXct(paste(dateTime$date, dateTime$time), "%Y-%m-%d %H", tz = timeZone)
    
    nwm_table = rbind(cbind("Forcast Type: ", "Medium"),
                      cbind("Forcast Date: ", dateTime$date),
                      cbind("Forcast Time: ", paste(dateTime$time, "UTC")),
                      cbind("Local Time Zone: ", timeZone),
                      cbind("Time Difference: ", dif)
    )
    colnames(nwm_table) = c("NWM", "Value")
    return(nwm_table)
}