#' @export

download_nomads_rda = function(fileList = NULL, number = 6, dir = NULL){
  if (is.null(dir)) {
    dir <- system.file("flowfinder", package = "FlowFinder")
  }
  
  dir.create(paste0(dir,'/data/current_nc'))
  
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
  
  # Write csv for info tab data
  dateTime <- data.frame(date = date, time = time, stringsAsFactors = FALSE)
  write.csv(dateTime, paste0(dir,'/data/current_nc/dateTime.csv'))
  
  map <- data.frame(tmp, stringsAsFactors = FALSE)
  #combine all vectors into a matrix
  colnames(map) <- c('num', 'min', 'max', 'filename')
  write.csv(map, paste0(dir,'/data/current_nc/map.csv'))
  
  
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
  inc = floor(len/25)
  start = 1
  end = start + inc - 1
  mappingList <- list()
  dir.create(paste0(dir,'/data/current_nc/changes'))
  
  message("Entering Loop!")
  
  # We are splitting the ~2.7 million COMIDS into 40 files
  for (i in 1:25) {
    end = start + inc - 1
    comids = nc_gd$var$streamflow$dim[[1]]$vals[start:end]
    df = data.frame(COMID = comids)
    # Grab a section of comids from each file
    for (j in 1:length(all.files)) {
      nc = ncdf4::nc_open(filename = all.files[j])
      vals = ncdf4::ncvar_get(nc, "streamflow", start = start, count = inc) * 35.3147
      df[, as.character(dates[j])] = vals
      ncdf4::nc_close(nc)
    }
    message(paste0("Combined group: ", i))
    # Get max values for each COMID over the time period
    df$max_date = colnames(df[,2:(number+1)])[max.col(df[,2:(number+1)],ties.method = 'first')]
    df$max = do.call(pmax, df[2:(number+1)])
    
    # Reshape data set
    Q = reshape2::melt(df, id.vars=c("COMID", "max", "max_date"))
    rm(df)
    Q$variable = lubridate::ymd_h(Q$variable)
    Q$max_date = lubridate::ymd_h(Q$max_date)
    colnames(Q) = c("COMID", "max", "max_date", "dateTime", "Q_cfs")
    rownames(Q) = NULL
    
    message("Reshaped!")
    
    # Encorporate data from monthly averages
    month = sprintf("%02d", as.numeric(format(Q$dateTime[1], format = "%m")))
    message(paste0('month:', month))
    #month_files = list.files("data", pattern = as.character(month), full.names = T)
    month_files = list.files(paste0(dir,"/data"), pattern = as.character(month), full.names = T)

    message(paste0('month_files:', month_files))
    
    norm = fst::read_fst(path = month_files)
    #norm = fst::read_fst(path = month_files)
    
    message("Normals Read!")
    
    #Q = merge(Q, norm, by = 'COMID')
    Q = dplyr::inner_join(Q, norm, by = 'COMID')
    rm(norm) 
    message("Removed norm!")
    names(Q) = c(head(names(Q), -1),"month_avg")
    Q$month_avg = Q$month_avg * 35.3147
    message("month avg created!")
    #Q$change <- with(Q, ifelse(month_avg == 0, 0, (Q_cfs - month_avg)/month_avg ))
    Q = dplyr::mutate(Q, change = ifelse(month_avg == 0, 0, (max - month_avg)/month_avg ))
    message("change created!")
    #Q$comp <-with(Q, ifelse(month_avg > Q_cfs, 'less', ifelse(month_avg < Q_cfs, 'greater', 'equal')))
    Q = dplyr::mutate(Q, comp = ifelse(month_avg > max, 'less', ifelse(month_avg < max, 'greater', 'equal')))
    message("comp created!")
    # Write fst file
    
  
    name = paste0(dir,"/data/current_nc/",month, "_", sprintf("%02d", i), ".fst")
    
    
    
    message("finding likely flood areas!")
    # Create `changes` data frame
    changes = dplyr::distinct(data.frame(COMID = Q$COMID,
                                         max = Q$max,
                                         change = Q$change,
                                         max_date = Q$max_date), .keep_all = TRUE)
    changes = changes[changes$change > 0,]
    fst::write_fst(changes, paste0(dir,'/data/current_nc/changes/',i,'.fst'), compress = 100)
    rm(changes)
    
    message("removing data sets!")
    
    # Remove unneeded columns from Q and write to file
    Q = Q[ ,!(names(Q) %in% c("max", "max_date", "change", "max_date"))]
    fst::write_fst(Q, path = name, compress = 100)
    rm(Q)
    
    message("mapping!")
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
  
  # Create file with potential flood areas
  change.files =  list.files(paste0(dir,'/data/current_nc/changes/'), pattern = ".fst$", full.names = T)
  max_increases = fst::read_fst(path = change.files[1])
  # Loop through all change files and create one data frame
  for (k in 2:length(change.files)) {
    max_increases = rbind(max_increases, fst::read_fst(path = change.files[k]))
  }
  # Sort by max flow and write file with top 5000
  max_increases = max_increases[order(max_increases$change, decreasing=TRUE), ]
  fst::write_fst(max_increases[1:1500,], paste0(dir,'/data/current_nc/max_increase.fst'), compress = 100)
  rm(max_increases)
  rm(change.files)
  gc()
  
  flood_map = make_flood_risk_map(path = paste0(dir,'/data/current_nc/max_increase.fst'))
  save(flood_map, file = paste0(dir,'/data/current_nc/flood_map.rda'))
}
