#Table 1: Station info
station_table <- function(values) {
  if(typeof(values$flow_data$nwis) == "S4") {
    station_data = cbind(paste0('<a href=',sprintf(
      "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$flow_data$nwis$site_no),' target="_blank">',values$flow_data$nwis$site_name,"</a>"),values$flow_data$nwis$site_no, round(values$flow_data$nwis$da_sqkm, digits = 0))
  } else {
    station_data = cbind('NA', 'NA', 'NA')
  }
  colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
  return(station_data)
}


# Table 2: Flowline info 
flowlines_table <- function(values) {
  max_order = max(values$flow_data$nhd@data$streamorde)
  table = rbind(cbind("Largest Stream Order: ", max_order),
                cbind("Largest Stream Name: ", values$flow_data$nhd@data$gnis_name[match(max_order, values$flow_data$nhd@data$streamorde)]),
                cbind("Number of Flowlines: ", length(values$flow_data$nhd)),
                cbind("Number of Water Bodies: ", length(values$flow_data$waterbodies)),
                cbind("Total Area (SqMi): ", size^2),
                cbind("Unique HUC8 units: ", paste(unique(as.numeric(na.omit(unique(substr(values$flow_data$nhd$reachcode,1,8))))), collapse = ", ")))
  colnames(table) = c('Hydrography', 'Value')
  return(table)
}

# Table 3: NWM info
nwm_table <- function(values) {
  dateTime = read.csv('data/current_nc/dateTime.csv', stringsAsFactors = FALSE)
  timeZones = fst::read.fst('data/comids_w_tz.fst')
  timeZone = timeZones[timeZones$COMID==values$flow_data$nhd$comid[1],]$tz
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