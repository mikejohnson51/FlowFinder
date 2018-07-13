#' @export

subset_nomads_rda_drop <- function(comids = NULL) {
  
  mapping = read.csv("data/current_nc/map.csv", stringsAsFactors = FALSE)
  
  files = c()
  for (id in comids) {
    files = c(files, subset(mapping, max >= id & min <= id)$filename)
  }
  files = unique(files)
  files <- files[!is.na(files)]
  
  res = list()
  for (file in files) {
    res_subset <- fst::read.fst(paste0('data/current_nc/', file))
    res_new = res_subset[res_subset$COMID %in% comids, ]
    res = rbind(res, res_new)
  }

  res$agency_code = "NOAA NWS"
  return(res)
}