subset_nomads_rda = function(comids = NULL) {
  
  if(!exists("nwm")){
    file = list.files("./flowline-app/data/current_nc", full.names = T)
    
    if(length(file) == 0){ 
      file = list.files("../flowline-app/data/current_nc", full.names = T)
    }
    nwm = fst::read_fst(file)
  }

  df = nwm[nwm$COMID %in% comids, ]
  df = df[order(df$COMID, df$dateTime),]
  return(df)

}

