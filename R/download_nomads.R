download_nomads = function(dir = './flowline-app/data/current_nc', fileList = NULL){

if(dir == "drop"){
  tmp = tempdir()
  for (i in seq_along(fileList[[3]])) {
    if (!file.exists(paste0(dir, "/", basename(fileList[[3]][i])))) {
      download.file(
        url = fileList[[3]][i],
        mode = "wb",
        destfile = paste0(tmp, "/", paste0(fileList[[1]], basename(fileList[[3]][i])))
      )
    }
  }
    
  all.files =  list.files(tmp, pattern = ".nc$", full.names = T)
  
  for(i in 1:length(all.files)){
    drop_upload(file = all.files[i], path = "current_nc/", dtoken = )
  }
  
  unlink(tmp, recursive = T)

  } else {
  
for (i in seq_along(fileList[[3]])) {
  if (!file.exists(paste0(dir, "/", basename(fileList[[3]][i])))) {
    download.file(
      url = fileList[[3]][i],
      mode = "wb",
      destfile = paste0(dir, "/", paste0(fileList[[1]], basename(fileList[[3]][i])))
    )
  }
}
}
message("Data Downloaded !")
}
