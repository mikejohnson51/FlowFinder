#' FlowFinder
#'
#' \code{FlowFinder} package
#' 
#' @docType package
#' @name FlowFinder
#'
#' @importFrom shinyjs click
#' 
#' @importFrom fst read_fst write_fst
#' 
#' @importFrom dplyr filter left_join select rename distinct mutate inner_join bind_rows summarise
#' 
#' @importFrom kableExtra footnote_marker_symbol kable_styling footnote column_spec
#' 
#' @importFrom knitr kable
#' 
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' 
#' @importFrom reshape2 melt
#' 
#' @importFrom utils download.file head write.csv read.csv
#' 
#' @importFrom AOI getAOI getBoundingBox aoiProj geocode describe
#' 
#' @importFrom sp SpatialPoints over
#' 
#' @importFrom leaflet.extras addDrawToolbar editToolbarOptions selectedPathOptions
#' 
#' @importFrom lutz tz_lookup_coords
#' 
#' @importFrom lubridate ymd_h ymd_hms with_tz
#' 
#' @importFrom purrr map_chr map map_lgl
#' 
#' @importFrom tidyr spread
#' 
#' @importFrom xts xts
#' 
#' @import shiny
#' 
#' @importFrom HydroData findNHD findNWIS findWaterbodies
#' 
#' @import ggplot2
#' 
#' @importFrom magrittr %>% 
#' 
#' @import leaflet
#' 
#' @import dygraphs


NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  { utils::globalVariables(c("comid",
                                                          "lengthkm",
                                                          "ftype",
                                                          "terminalfl",
                                                          "fromnode", "tonode",
                                                          "totdasqkm", 
                                                          "startflag",
                                                          "streamorde",
                                                          "streamcalc",
                                                          "terminalpa",
                                                          "pathlength",
                                                          "divergence",
                                                          "comid.y",
                                                          "toCOMID",
                                                          "files",
                                                          "head"))
}
