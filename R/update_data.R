#' @export
#' 

source("R/get_nomads_filelist.R")
source("R/download_nomads_rda.R")
source("R/flood_risk_map.R")

update_data = function() {
  fileList = get_nomads_filelist(num  = 40)
  download_nomads_rda(fileList, number = 40)
}