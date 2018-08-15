#' @export

download_nomads_rda = function(fileList = NULL, number = 6, dir = NULL){
  if (is.null(dir)) {
    dir <- system.file("flowfinder", package = "FlowFinder")
  }
  
  dir.create(paste0(dir,'/data/current_nc_new'))
  
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
  
  month = sprintf("%02d", lubridate::month(date))
  month_files = list.files(paste0(dir,"/data"), pattern = as.character(month), full.names = T)
  norm = fst::read_fst(path = month_files)
  colnames(norm) <- c("COMID", "month_avg")
  
  
  # Write csv for info tab data
  dateTime <- data.frame(date = date, time = time, stringsAsFactors = FALSE)
  write.csv(dateTime, paste0(dir,'/data/current_nc_new/dateTime.csv'))
  
  if(length(grep(all.files, pattern = "medium")) > 1){
    interval = 3
  } else {
    interval = 1
  }
  
  dates = NULL
  
  for(i in 1:number){
    dates = append(dates,  format(lubridate::ymd_hms(paste0(date, "-", time, "-00-00"), tz = 'GMT') + ((((i) * interval)) * 60 * 60), tz ="GMT", format="%Y-%m-%d  %H"))
  }
  
  # Opening first file to get necessary data
  nc_gd <- ncdf4::nc_open(filename = all.files[1])
  len = nc_gd[["var"]][["streamflow"]][["varsize"]]
  inc = floor(len/25)
  start = 1
  end = start + inc - 1
  mappingList <- list()
  dir.create(paste0(dir,'/data/current_nc_new/changes'))
  
  message("Entering Loop!")
  
  # We are splitting the ~2.7 million COMIDS into 40 files
  for (i in 1:25) {
    end = start + inc - 1
    comids = nc_gd$var$streamflow$dim[[1]]$vals[start:end]
    df <- matrix(ncol=(length(all.files)+1), nrow=length(comids))
    df[,1] <- comids
    # Grab a section of comids from each file
    for (j in 1:length(all.files)) {
      nc = ncdf4::nc_open(filename = all.files[j])
      vals = ncdf4::ncvar_get(nc, "streamflow", start = start, count = inc) * 35.3147
      #df[, as.character(dates[j])] = vals
      df[,(j+1)] <- vals
      ncdf4::nc_close(nc)
    }
    
    df = data.frame(df)
    colnames(df) <- c("COMID", dates)
    
    # Calculate and save changes 
    increases <- df %>%
      dplyr::mutate(
        max_date = colnames(df[,2:(number+1)])[max.col(df[,2:(number+1)],ties.method = 'first')],
        max_val = do.call(pmax, df[2:(number+1)])
      ) %>% 
      dplyr::inner_join(norm, by = 'COMID') %>% 
      dplyr::mutate(
        month_avg = month_avg * 35.3147,
        month_avg = replace(month_avg, month_avg==0, 0.001),
        change = (max_val - month_avg)/month_avg
      ) %>% 
      dplyr::filter(max_val >= 25 & max_val > month_avg )
    
    fst::write_fst(increases, path = paste0(dir,'/data/current_nc_new/changes/',i,'.fst'))
    
    # Reshape data frame: wide -> long
    Q <- reshape2::melt(df, id.vars=c("COMID")) %>% 
      dplyr::mutate(variable = lubridate::ymd_h(variable)) %>% 
      dplyr::rename(dateTime = variable, Q_cfs = value)
    
    # Write file with data
    name = paste0(dir,"/data/current_nc_new/",month, "_", sprintf("%02d", i), ".fst")
    fst::write_fst(Q, path = name, compress = 100)
    rm(Q)
    
    # Get min and maxs for mapping
    min = min(comids)
    max = max(comids)
    mappingList[[i]] <- c(i,min,max, as.character(paste0(month, "_", sprintf("%02d", i), ".fst")))
    start = end +1
    
    message(paste("Completed: ", i))
  }
  
  # Close and remove unneeded files
  ncdf4::nc_close(nc_gd)
  unlink(list.files(tmp, pattern = ".nc$", full.names = TRUE))
  
  
  #combine all vectors into a matrix
  tmp = do.call("rbind",mappingList)
  map <- data.frame(tmp, stringsAsFactors = FALSE)
  colnames(map) <- c('num', 'min', 'max', 'filename')
  write.csv(map, paste0(dir,'/data/current_nc_new/map.csv'))
  
  find_max_increases(dir = dir)
  create_high_flow_map(dir = dir)
  
  # Replace old data with new data
  unlink(paste0(dir,'/data/current_nc'), recursive=TRUE)
  file.rename(paste0(dir,'/data/current_nc_new/'), 'data/current_nc')
  
}


find_max_increases <- function(number = 1500, dir = NULL) {
  
  # Get file list
  change.files =  list.files(paste0(dir,'/data/current_nc_new/changes/'), pattern = ".fst$", full.names = T)
  
  max_increases = data.frame()
  
  for (k in 1:length(change.files)) {
    max_increases = dplyr::bind_rows(max_increases, fst::read_fst(path = change.files[k]))
  }
  
  max_increase <- dplyr::select(max_increases, COMID, max_val, change, max_date) %>% 
    dplyr::arrange(dplyr::desc(change))  %>% 
    dplyr::filter(row_number() <= number)
  
  fst::write_fst(max_increase, path = paste0(dir,'/data/current_nc_new/max_increase.fst'))
  
}

create_high_flow_map <- function(dir = NUll) {
  
  # Make and save flood map
  flood_map = make_flood_risk_map(path = paste0(dir,'/data/current_nc_new/max_increase.fst'))
  base::save(flood_map, file = paste0(dir,'/data/current_nc_new/flood_map.rda'))

}










