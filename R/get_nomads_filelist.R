#' Getting Most Current NWM data from the NOAA NOMADs server
#'
#'This function accesses the NOAA Nomads server to download the most up to date forecast NetCDF files.
#'These files cover the spatial domain of the continential United States. These files will be stored in the
#''./FlowFinder' subfolder of the specified directory. Each time this function is run the old files will be removed.
#' So, if you see something interesting be sure to save the data elsewhere!
#'
#' @author
#' Mike Johnson
#' @export
#' @return
#' This fuction saves the 18 NetCDF files related to the most recent forecasts.

get_nomads_filelist = function(type = NULL,
                               time = NULL,
                               num = 6) {

  date = format(strptime(format(Sys.time(), tz = "GMT"),format = "%Y-%m-%d"), format = "%Y-%m-%d")
  backup.date = as.Date(date) -1
  #if (is.null(time)) {
  #  time = format(strptime(format(Sys.time(), tz = "GMT"),format = "%Y-%m-%d") - 10800, format = "%H")
  #  startTime = time
  #}

  if (is.null(type)) {
    type = "medium_range"
  }

  base.url <-
    paste0(
      "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.",
      gsub("-", "", date),
      "/",
      type,
      "/"
    ) ## set base URL

tryCatch({
    files <<- suppressMessages(readLines(base.url))
  },
  error=function(error_message) {
    trycatch({base.url <<-
      paste0(
        "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.",
        gsub("-", "", backup.date),
        "/",
        type,
        "/"
      )
    
    files <<-suppressMessages(readLines(base.url))
    date  <<- backup.date
    
  })},
  
  warning=function(warning_message) {
    base.url <<-
      paste0(
        "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.",
        gsub("-", "", backup.date),
        "/",
        type,
        "/"
      )
    
    files <<- suppressMessages(readLines(base.url))
    date <<- backup.date
    
  }
  
)

  fileList = lapply(regmatches(files, gregexpr('(\").*?(\")', files, perl = TRUE)), function(y)
    gsub("^\"|\"$", "", y)) ## subset file names

  fileList = fileList[grep("channel", fileList)] ## Extract channel data

  if (type == "medium_range") {
    interval = 6

    for (i in seq(18, 0,-6)) {
      time = paste0("t", sprintf("%02d", i))
      fileList_time = fileList[grep(time, fileList)] ## Extract time of interest
      if (length(fileList_time) != 0) {
        break
      }
    }
  }

  if (type == "short_range") {
    interval = 1
    #time = strptime(format(Sys.time(), tz = "GMT"), format = "%Y-%m-%d %H:%M:%S")$hour

    for (i in seq(time, 0,-1)) {
      time = paste0("t", sprintf("%02d", i))
      fileList_time = fileList[grep(time, fileList)] ## Extract time of interest
      if (length(fileList_time) != 0) {
        break
      }
    }
  }

  fileList.fin = head(fileList_time, num) # Limit number
  urls = paste0(base.url, fileList.fin)
  
  time = gsub('^.*m.t\\s*|\\s*z.*$', '', basename(fileList.fin[[1]]))
  
  return(
    list(date = date,
         startTime = time,
         urls = urls)
    )
}

 

  

