#' @export
#' 

update_data = function() {
  fileList = FlowFinder::get_nomads_filelist(num  = 40)
  FlowFinder::download_nomads_rda(fileList, number = 40)
}
