download_nomads = function(dir = './flowline-app/data/current_nc', fileList = NULL){

for (i in seq_along(fileList[[2]])) {
  if (!file.exists(paste0(dir, "/", basename(fileList[[2]])[i]))) {
    download.file(
      url = fileList[[2]][i],
      mode = "wb",
      destfile = paste0(dir, "/", paste0(fileList[[1]], basename(fileList[[2]])[i]))
    )
  }
}
message("Data Downloaded !")
}
