#' @export

subset_nomads_rda <- function(comids = NULL, file = NULL) {
  n_rows = fst::metadata_fst(path = file)$nrOfRows
  inc = ceiling(n_rows/12)
  df = list()
  i = 1
  while (i <= n_rows) {
    df_subset <- fst::read.fst(file, from = i, to = i+inc)
    df_new = df_subset[df_subset$COMID %in% comids, ]
    rm(df_subset)
    df_new = df_new[order(df_new$COMID, df_new$dateTime),]
    df = rbind(df, df_new)
    i = i+inc
  }
  message("Successfully loaded data !")
  return(df)
}
