#' @export

download_nomads_rda = function(fileList = NULL, number = 6, dir = NULL){
  if (is.null(dir)) {
    dir <- system.file("flowlinefinder", package = "FlowlineFinder")
  }
  
  tmp = tempdir()
  for (i in seq_along(fileList[[3]])) {
    download.file(
      url = fileList[[3]][i],
      mode = "wb",
      destfile = paste0(tmp, "/", paste0(fileList[[1]], basename(fileList[[3]][i])))
    )
  }
  all.files =  list.files(tmp, pattern = ".nc$", full.names = T)
  
  date  = fileList$date
  time = fileList$startTime
  rm(fileList)
  
  if(length(grep(all.files, pattern = "medium")) > 1){
    interval = 3
  } else {
    interval = 1
  }
  dates = NULL
  
  for(i in 1:number){
    dates = append(dates,  format(lubridate::ymd_hms(paste0(date, "-", time, "-00-00"), tz = 'GMT') + ((((i) * interval)) * 60 * 60), tz ="GMT", format="%Y-%m-%d  %H"))
  }
  message("Data Downloaded !")
  
  # Opening first file to get necessary data
  nc_gd <- ncdf4::nc_open(filename = all.files[1])
  len = nc_gd[["var"]][["streamflow"]][["varsize"]]
  inc = floor(len/40)
  start = 1
  end = start + inc - 1
  mappingList <- list()
  
  # We are splitting the ~2.7 million COMIDS into 40 files
  for (i in 1:40) {
    end = start + inc - 1
    comids = nc_gd$var$streamflow$dim[[1]]$vals[start:end]
    df = data.frame(COMID = comids)
    # Grab a section of comids from each file
    for (j in 1:length(all.files)) {
      nc = ncdf4::nc_open(filename = all.files[j])
      vals = ncvar_get(nc, "streamflow", start = start, count = inc) * 35.3147
      df[, as.character(dates[j])] = vals
      ncdf4::nc_close(nc)
    }
    # Reshape data set
    Q = reshape2::melt(df, id.vars=c("COMID"))
    rm(df)
    Q$value = Q$value
    Q$variable = lubridate::ymd_h(Q$variable)
    Q$agency_code = "NOAA"
    colnames(Q) = c("COMID", "dateTime", "Q_cfs", "agency_code")
    rownames(Q) = NULL
    # Write fst file
    month = as.numeric(format(Q$dateTime[1], format = "%m"))
    month = sprintf("%02d", month)
    name = paste0(dir,"/data/current_nc/",month, "_", sprintf("%02d", i), ".fst")
    fst::write_fst(Q, path = name)
    # Clear from memory
    rm(Q)
    # Get min and maxs for mapping
    min = min(comids)
    max = max(comids)
    mappingList[[i]] <- c(i,min,max, as.character(paste0(month, "_", sprintf("%02d", i), ".fst")))
    start = end +1
  }
  ncdf4::nc_close(nc_gd)
  unlink(list.files(tmp, pattern = ".nc$", full.names = TRUE))
  tmp = do.call("rbind",mappingList)
  map <- data.frame(tmp, stringsAsFactors = FALSE)
  #combine all vectors into a matrix
  colnames(map) <- c('num', 'min', 'max', 'filename')
  write.csv(map, paste0(dir,'/data/current_nc/map.csv'))
  message(paste0(name," finished!"))
  gc()
}
