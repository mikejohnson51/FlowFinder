subset_nomads_rda = function(rda = "./flowline-app/data/current_nc/flows.rda", comids = NULL) {
  
  if(!exists("nwm")){
    load(rda)
  }

  Q = nwm$flow
  
  df = Q[Q$COMID %in% comids, ]
  
  df = df[order(df$COMID, df$dateTime),]
  rownames(df) = NULL

  return(df)
  
}

