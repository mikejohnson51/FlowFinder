subset_nomads = function(dir = "./flowline-app/data/current_nc", comids = NULL) {
  
  
  if (class(comids) == 'numeric') {
    comids = comids
    shp = NULL
  } else if (class(comids) == 'SpatialLinesDataFrame') {
    shp = comids
    comids = shp$comid
  } else if (class(comids) == 'SpatialPolygonsDataFrame' |
             class(comids) == "SpatialPolygons") {
    shp = findNHD(clip_unit = comids)
    comids = shp$comid
  }

all.files = list.files(dir, full.names = TRUE)
    
    if (length(grep(all.files, pattern = "medium")) > 1) {
    } interval = 6
  } else {
    interval = 1
  }
  nc <- nc_open(filename = all.files[1])
  
  comids.all = nc$var$streamflow$dim[[1]]$vals
  
  comids_of_value = comids[comids %in% comids.all]
  
  start <- vector(mode = "numeric", length(comids_of_value))
  
  for (i in 1:length(comids_of_value)) {
    start[i] = which(comids.all == comids_of_value[i])
  }
  
  nc_close(nc)
  
  df = NULL
  
  for (i in seq_along(all.files)) {
    nc = nc_open(filename = all.files[i])
    values = ncvar_get(nc, varid = "streamflow")
    
    if (!is.null(dir)) {
      for (j in 1:length(start)) {
        df =  rbind(
          df,
          data.frame(
            "NOAA",
            comids_of_value[j],
            lubridate::ymd_hms(paste0(date, "-", 15, "-00-00"), tz = 'UTC') + ((((i -
                                                                                    1) * interval
            )) * 60 * 60),
            values[start[j]],
            stringsAsFactors = FALSE
          )
        )
      }
    }
    
    nc_close(nc)
  }
  
  if (!is.null(dir)) {
    colnames(df) = c("agency_cd",
                     "comid",
                     "dateTime",
                     "cms")
  }
  
  df = df[order(df$comid, df$dateTime),]
  
  return(df)
}
