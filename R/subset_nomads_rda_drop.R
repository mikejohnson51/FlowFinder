#' @export

subset_nomads_rda_drop <- function(comids = NULL, mapping = NULL) {
  
  groups = c()
  uid = sample(1:1000000, 1)
  dir.create(file.path(tempdir(), as.character(uid)), showWarnings = FALSE)
  #tmp = paste0(tempdir(),"/",as.character((uid)))
  for (i in 1:40) {
    min = mapping[i,]$min
    max = mapping[i,]$max
    file = mapping$filename[i]
    file = mapping[i,]$filename
    for (id in comids) {
      if (min <= id && id <= max) {
        groups = c(groups, file)
      }
    }
  }
  groups = unique(groups)
  res = list()
  for (group in groups) {
    #local = paste0(tmp, "/",group,".fst" )
    #start.time <- Sys.time()
    #rdrop2::drop_download(name, local_path = local)
    #print("Downloaded From Dropbox!")
    #message("Downloaded From Dropbox! (message)")
    name = paste0('data/current_nc/', group)
    res_subset <- fst::read.fst(name)
    res_new = res_subset[res_subset$COMID %in% comids, ]
    res = rbind(res, res_new)
  }
  message("Successfully loaded data !")
  #print(tmp)
  #unlink(tmp, recursive = TRUE)
  return(res)
}