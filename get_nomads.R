#' Getting Most Current NWM data from the NOAA NOMADs server
#'
#'This function accesses the NOAA Nomads server to download the most up to date forecast NetCDF files.
#'These files cover the spatial domain of the continential United States. These files will be stored in the
#''./FlowlineFinder' subfolder of the specified directory. Each time this function is run the old files will be removed.
#' So, if you see something interesting be sure to save the data elsewhere!
#'
#' @examples
#' get_nomads(dir = "mikejohnson/desktop")
#' @author
#' Mike Johnson
#' @export
#' @return
#' This fuction saves the 18 NetCDF files related to the most recent forecasts.

get_nomads = function(dir = "./flowline-app/data/current_nc",
                      type = NULL,
                      time = NULL,
                      num = 6) {

  dir = normalizePath(dir)
  if (!dir.exists(paste0(dir, "/current_nc"))) {
    dir.create(paste0(dir, "/current_nc"))
  }

  dir = paste0(dir, "/current_nc")

  date = format(strptime(format(Sys.time(), tz = "GMT"),format = "%Y-%m-%d") - 10800, format = "%Y-%m-%d")
  

  if (is.null(time)) {
    time = format(strptime(format(Sys.time(), tz = "GMT"),format = "%Y-%m-%d") - 10800, format = "%H")
    startTime = time
  }

  if (is.null(type)) {
    type = "medium_range"
  }

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

  base.url <-
    paste0(
      "http://nomads.ncep.noaa.gov/pub/data/nccf/com/nwm/prod/nwm.",
      gsub("-", "", date),
      "/",
      type,
      "/"
    ) ## set base URL

files <- tryCatch({
    suppressMessages(readLines(base.url))
  },
  error=function(error_message) {
    stop("No new forecast for this time")
  }, 
  
  warning=function(w) {
    stop("No new forecast for this time")
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

  fileList.fin = head(fileList_time, num) # Limit to Two day

  urls = paste0(base.url, fileList.fin)

  for (i in seq_along(urls)) {
    if (!file.exists(paste0(dir, "/", fileList.fin[i]))) {
      download.file(
        url = urls[i],
        mode = "wb",
        destfile = paste0(dir, "/", fileList.fin[i])
      )
    }
  }
  message("Data Downloaded !")
}

  

