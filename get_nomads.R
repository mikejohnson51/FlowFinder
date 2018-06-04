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

get_nomads = function(dir = NULL,
                      type = NULL,
                      time = NULL,
                      comids = NULL) {

  dir = normalizePath(dir)
  if (!dir.exists(paste0(dir, "/FlowlineFinder"))) {
    dir.create(paste0(dir, "/FlowlineFinder"))
  }
  dir = paste0(dir, "/FlowlineFinder")

  date = format(Sys.Date(), tz = "GMT")

  if (is.null(time)) {
    time = strptime(format(Sys.time(), tz = "GMT"), format = "%Y-%m-%d %H:%M:%S")$hour
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

  fileList = lapply(regmatches(readLines(base.url), gregexpr('(\").*?(\")', files, perl = TRUE)), function(y)
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

  fileList.fin = head(fileList_time, 6) # Limit to Two day

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


  if (!is.null(comids)) {
    all.files = list.files(dir, full.names = TRUE)
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

      } else{
        forecast_hour = substring(sub(".*tm", "", all.files[i]), 1, 2)

        for (j in 1:length(start)) {
          df =  rbind(
            df,
            data.frame(
              "NOAA",
              comids_of_value[j],
              as.POSIXct(
                gsub("_", " ", ncatt_get(nc, 0)$model_initialization_time)
              ),
              values[start[j]],
              forecast_hour,
              "UTC",
              stringsAsFactors = FALSE
            )
          )
        }
      }
      nc_close(nc)
    }

    if (!is.null(dir)) {
      colnames(df) = c("agency_cd", "comid", "dateTime", "cms")
    } else {
      colnames(df) = c("agency_cd",
                       "comid",
                       "dateTime",
                       "cms",
                       "Latency",
                       "tz_cd")
    }

    df = df[order(df$comid, df$dateTime), ]
  }

  return(df)
}
