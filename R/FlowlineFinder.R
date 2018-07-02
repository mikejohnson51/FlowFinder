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
#' @import shiny



NULL


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  { utils::globalVariables(c("LAT", "LON",
                                                          "State", "Longitude", "Latitude",
                                                          "nid_cleaned",
                                                          "usgsStations",
                                                          "snotel",
                                                          "daymet_tiles",
                                                          "kopRas",
                                                          "DAY", "site_no", "YEAR", "MONTH",
                                                          "year_2000", "year_2005", "year_2010", "COUNTY", "ID",
                                                          "PARAMETER", "Date", ".", "ap"))
}
