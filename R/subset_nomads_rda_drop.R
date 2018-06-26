#' @export

subset_nomads_rda_drop <- function(comids = NULL, mapping = NULL) {
  groups = c()
  uid = sample(1:1000000, 1)
  dir.create(file.path(tempdir(), as.character(uid)), showWarnings = FALSE)
  tmp = paste0(tempdir(),"/",as.character((uid)))
  for (i in 1:50) {
    min = mapping[i,]$min
    max = mapping[i,]$max
    for (id in comids) {
      if (min <= id && id <= max) {
        groups = c(groups, i)
      }
    }
  }
  groups = unique(groups)
  res = list()
  for (group in groups) {
    name = paste0("current_nc/",group,".fst")
    local = paste0(tmp, "/",group,".fst" )
    start.time <- Sys.time()
    drop_download(name, local_path = local)
    res_subset <- fst::read.fst(local)
    res_new = res_subset[res_subset$COMID %in% comids, ]
    res = rbind(res, res_new)
  }
  message("Successfully loaded data !")
  print(tmp)
  unlink(tmp, recursive = TRUE)
  return(res)
}