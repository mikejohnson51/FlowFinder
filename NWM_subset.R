comid_nc = nc_open("/Users/mikejohnson/Downloads/nwm.t00z.medium_range.channel_rt.f003.conus.nc")

comids.all = comid_nc$var$streamflow$dim[[1]]$vals



all.files = list.files("/Users/mikejohnson/Desktop/normals", full.names = TRUE)[1:12]
jan = nc_open(all.files[1])

sub_comid = jan$dim$feature_id$vals

normals = data.frame(COMID = sub_comid)

for (i in 1:12){
  tmp = nc_open(all.files[i])
  normals[,month.abb[i]] = ncvar_get(tmp, "streamflow")
  nc_close(tmp)
}


fst::write_fst(normals, path = "/Users/mikejohnson/Documents/GitHub/FlowlineFinder/flowline-app/data/normals.fst", 100)


norm = fst::read_fst(path = "/Users/mikejohnson/Documents/GitHub/FlowlineFinder/flowline-app/data/normals.fst")

for(i in 1:12){
  tmp = norm[,c(1,i+1)]
  fst::write_fst(tmp, path = paste0("/Users/mikejohnson/Documents/GitHub/FlowlineFinder/flowline-app/data/n",sprintf("%02d", i), ".fst"), 100)
}

jan = norm[,c(1,i+1)]
  
  fst::write_fst(norm, path = "/Users/mikejohnson/Documents/GitHub/FlowlineFinder/flowline-app/data/011.fst", 100)

