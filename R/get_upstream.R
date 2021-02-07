#' @export

get_upstream <- function(flines) {
  dplyr::select(flines, COMID) %>% 
    dplyr::left_join(
      dplyr::select(flines, COMID, toCOMID),
      by = c("COMID" = "toCOMID")
    ) %>%
    dplyr::rename(fromCOMID = COMID.y)
}
