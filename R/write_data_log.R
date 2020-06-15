write_data_log <- function(dataframe, trackr_dir, file_hash){
  
  trackr_fn <- paste0(trackr_dir, '/', file_hash, '_dl.json')
  
  trackr_json <- jsonlite::toJSON(dataframe)
  
  write(trackr_json, file = trackr_fn)
  
  print(paste0('Successfully written trackr data log ', tail(stringr::str_split(trackr_fn, '/')[[1]], 1)))
  
}