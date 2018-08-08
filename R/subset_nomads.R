#' @export

subset_nomads <- function(comids = NULL) {
  
  comids = as.numeric(comids)
  
  mapping = read.csv("data/current_nc/map.csv", stringsAsFactors = FALSE)
  
  files = unique(sapply(comids, function(id) subset(mapping, max >= id & min <= id)$filename)) 
  
  res = list()
  for (file in files) {
    res_subset <- fst::read.fst(paste0('data/current_nc/', file))
    res_new = dplyr::filter(res_subset, COMID %in% comids)
    res = dplyr::bind_rows(res, res_new)
  }

  res$agency_code = "NOAA NWS"
  return(res)
}






