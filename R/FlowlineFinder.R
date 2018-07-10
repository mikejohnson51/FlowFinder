#' FlowlineFinder
#'
#' \code{FlowlineFinder} package
#' 
#' @docType package
#' @name FlowlineFinder
#'
#' @importFrom shinyjs click
#' 
#' @importFrom fst read_fst write_fst
#' 
#' @importFrom dplyr filter left_join select rename distinct mutate inner_join
#' 
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' 
#' @importFrom lubridate ymd_hms
#' 
#' @importFrom reshape2 melt
#' 
#' @importFrom rdrop2 drop_upload drop_read_csv drop_download 
#' 
#' @importFrom utils download.file head write.csv
#' 
#' @importFrom AOI getAOI getPoint
#' 
#' @importFrom xts as.xts
#' 
#' @import shiny
#' 
#' @import HydroData
#' 
#' @import ggplot2
#' 
#' @import magrittr
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
