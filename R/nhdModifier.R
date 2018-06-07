
prep_nhd <- function(flines, min_network_size = 2, min_path_length = 1, purge_non_dendritic = TRUE) {
  if(grepl("Spatial",class(flines))) {
    message("removing geometry")
    flines <- flines@data
  }
  
  orig_rows <- nrow(flines)
  
  flines <- select(flines, comid, lengthkm, ftype, terminalfl,
                   fromnode, tonode, totdasqkm, startflag,
                   streamorde, streamcalc, terminalpa, pathlength, divergence)
  
  if(purge_non_dendritic) {
    flines <- filter(flines, ftype != "Coastline" & # Remove Coastlines
                       streamorde == streamcalc) #& # Also use streamorder and streamcalc to select only the main paths.
  } else {
    flines <- filter(flines, ftype != "Coastline") # Remove Coastlines
    flines[["fromnode"]][which(flines$divergence == 2)] <- NA
  }
  
  terminal_filter <- flines$terminalfl == 1 & flines$totdasqkm < min_network_size
  start_filter <- flines$startflag == 1 & flines$pathlength < min_path_length
  
  if(any(terminal_filter) | any(start_filter)) {
    
    tiny_networks <- rbind(filter(flines, terminal_filter),
                           filter(flines, start_filter))
    
    flines <- filter(flines, !flines$terminalpa %in% unique(tiny_networks$terminalpa))
  }
  
  warning(paste("Removed", orig_rows - nrow(flines), "flowlines that don't apply.\n",
                "Includes: Coastlines, non-dendritic paths, \nand networks",
                "with drainage area less than",
                min_network_size, "sqkm"))
  
  # Join ToNode and FromNode along with COMID and Length to get downstream attributes.
  flines <- left_join(flines, select(flines, toCOMID = comid, fromnode), by = c("tonode" = "fromnode"))
  
  #if(!all(flines[["terminalfl"]][which(is.na(flines$toCOMID))] == 1)) {
  #  stop("FromNode - ToNode imply terminal flowlines that are not\n flagged terminal.",
  #       "Can't assume NA toCOMIDs go to the ocean.")
  #}
  
  select(flines, -tonode, -fromnode, -terminalfl, -startflag,
         -streamorde, -streamcalc, -terminalpa, -ftype, -pathlength, -divergence)
}


get_upstream <- function(flines) {
  left_join(select(flines, comid), select(flines, comid, toCOMID),
            by = c("comid" = "toCOMID")) %>%
    rename(fromCOMID = comid.y)
}
