#' @export
#' 

update_data = function() {
  fileList = FlowlineFinder::get_nomads_filelist(num  = 4)
  FlowlineFinder::download_nomads_rda(fileList, number = 4)
}