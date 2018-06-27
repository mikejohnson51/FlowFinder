#' @export

get_upstream <- function(flines) {
  dplyr::left_join(dplyr::select(flines, comid), dplyr::select(flines, comid, toCOMID),
                   by = c("comid" = "toCOMID")) %>%
    dplyr::rename(fromCOMID = comid.y)
}