download_nomads = function(dir = './flowline-app/data/current_nc', fileList){

for (i in seq_along(fileList)) {
  if (!file.exists(paste0(dir, "/", basename(fileList)[i]))) {
    download.file(
      url = fileList[i],
      mode = "wb",
      destfile = paste0(dir, "/", basename(fileList)[i])
    )
  }
}
message("Data Downloaded !")
}
