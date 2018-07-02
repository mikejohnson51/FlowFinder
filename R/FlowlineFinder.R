#' FlowlineFinder
#'
#' \code{FlowlineFinder} package
#' 
#' @docType package
#' @name FlowlineFinder
#'
#' @importFrom dismo geocode
#' 
#' @importFrom shinyjs click
#' 
#' @importFrom fst read_fst write_fst
#' 
#' @importFrom DT datatable dataTableAjax renderDataTable
#' 
#' @importFrom dplyr filter left_join select rename
#' 
#' @importFrom ncdf4 nc_open ncvar_get nc_close
#' 
#' @importFrom rdrop2 drop_upload drop_read_csv drop_download 
#' 
#' @importFrom utils download.file head write.csv 
#' 
#' @import shiny



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
