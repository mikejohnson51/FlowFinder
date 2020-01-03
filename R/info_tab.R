#Table 1: Station info
station_table <- function(values) {
  if(typeof(values) == "S4") {
    station_data = cbind(paste0('<a href=',sprintf(
                            "https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",values$site_no),' target="_blank">',values$site_name,"</a>"),
                         values$site_no, 
                         round(values$da_sqkm, digits = 0)
                         )
  } else {
    station_data = cbind('NA', 'NA', 'NA')
  }
  colnames(station_data) = c("USGS Site", "Site No.", "Drainage Area (SqKm)")
  return(station_data)
}

# Table 2: Flowline info 
flowlines_table <- function(values, area, waterbodies) {
  
  max_order = ifelse(!is.null(values), max(values@data$streamorde), "NA")
  stream_name = ifelse(!is.null(values), values@data$gnis_name[match(max_order, values@data$streamorde)], "NA")
  num_flowlines = length(values)
  unique_huc8 = ifelse(!is.null(values), paste(unique(as.numeric(na.omit(unique(substr(values$reachcode,1,8))))), collapse = ", "), "NA")
  
  table = data.frame(
    "Hydrography" = c("Largest Stream Order", 
                      "Largest Stream Name",
                      "Number of Flowlines",
                      "Number of Water Bodies",
                      "Total Area (SqMi)",
                      "Unique HUC8 units"),
    "Value" = c(max_order, 
                stream_name,
                num_flowlines,
                waterbodies,
                area,
                unique_huc8)
  ) %>% mutate(Hydrography = paste0(Hydrography,": "))

  return(table)
}

get_local_time <- function(dateTime, timezone) {
  return(as.POSIXct(paste(dateTime$date, dateTime$time), "%Y-%m-%d %H", tz = timezone))
}

# Table 3: NWM info
nwm_table <- function(timezone = NULL) {
    dateTime = read.csv('data/current_nc/dateTime.csv', stringsAsFactors = FALSE)
    
    dif <- get_local_time(dateTime = dateTime, timezone = "GMT") -
      get_local_time(dateTime = dateTime, timezone = timezone)
    
    table <- data.frame(
      "NWM" = c("Forcast Type",
                "Forcast Date",
                "Forcast Time",
                "Local Time Zone",
                "Time Difference"),
      "Value" = c("Medium",
                  dateTime$date,
                  paste(dateTime$time, "UTC"),
                  timezone,
                  dif)
    ) %>% mutate(NWM = paste0(NWM,": "))
    
    return(table)
}