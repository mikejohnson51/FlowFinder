download_nomads_rda = function(fileList = NULL, dropbox = FALSE){
  
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
  
  if(length(grep(all.files, pattern = "medium")) > 1){
    interval = 3
  } else {
    interval = 1
  }
  dates = NULL
  
  for(i in 1:6){
    dates = append(dates,  format(lubridate::ymd_hms(paste0(date, "-", time, "-00-00"), tz = 'GMT') + ((((i) * interval)) * 60 * 60), tz ="GMT", format="%Y-%m-%d  %H"))
  }
  
  message("Data Downloaded !")
  
  nc <- nc_open(filename = all.files[1])
  comids.all = nc$var$streamflow$dim[[1]]$vals
  values = ncvar_get(nc, varid = "streamflow") * 35.3147
  df = data.frame(COMID = comids.all)
  df[,as.character(dates[1])] = values
  for (i in 2:length(all.files)) {
    nc = nc_open(filename = all.files[i])
    vals = ncvar_get(nc, varid = "streamflow") * 35.3147
    df[, as.character(dates[i])] = vals
    nc_close(nc)
  }
  Q = reshape2::melt(df, id.vars=c("COMID"))
  Q$value = Q$value
  Q$variable = lubridate::ymd_h(Q$variable)
  Q$agency_code = "NOAA"
  colnames(Q) = c("COMID", "dateTime", "Q_cfs", "agency_code")
  rownames(Q) = NULL
  nwm = list(
    date = date,
    time = time,
    forecast = 'medium',
    flow = Q
  )
  name =paste0(date,"_",time,"_medium_.fst")
  
  fst::write.fst(Q,  path = paste0("./inst/flowlinefinder/data/current_nc/", name), 100) 
  
  unlink(list.files(tmp, pattern = ".nc$", full.names = TRUE))
  if (dropbox) {
    mylist <- list() #create an empty list
    unique = unique(Q$COMID)
    sp = split(unique, sort(unique%%50))
    i = 1
    for (x in sp) {
      min = min(x)
      max = max(x)
      mylist[[i]] <- c(i,min,max)
      subset = Q[Q$COMID >= min & Q$COMID <= max,]
      subset.Q = as.data.frame(subset)
      name = paste0("./inst/flowlinefinder/data/current_nc/",i, ".fst")
      write_fst(subset.Q, path = name)
      drop_upload(name, path = "current_nc")
      unlink(name)
      i = i + 1
    }
    map <- do.call("rbind",mylist) #combine all vectors into a matrix
    colnames(map) <- c('num', 'min', 'max')
    write.csv(map, "./inst/flowlinefinder/data/current_nc/map.csv")
    drop_upload("./inst/flowlinefinder/data/current_nc/map.csv", path = "current_nc")
  }
  
  
  message(paste0(name," finished!"))
}
