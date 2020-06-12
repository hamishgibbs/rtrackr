# write_new_trackr_file
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
# @param file_hash character, hash of current file row hashes
# @param trackr_dir path, path to store log file
# @param timepoint_message optional character, message to identify timepoint - similar to a git commit message
#
# @importFrom jsonlite toJSON

write_new_trackr_file <- function(hash_string, file_hash, timepoint_message, trackr_dir){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  tstamp <- as.numeric(Sys.time())
  hashes <- list()
  
  for(i in 1:length(hash_string %>% pull(hash))){
    
    hash <- hash_string$hash[i]
    
    hashes[[i]] = list(type = 'root', hash = hash, parent_hash = 'None')
    
  }
  
  hashes = list(parent_file = 'None', timestamp = tstamp, timepoint_message = timepoint_message, trackr_ids = hashes)
  trackr_fn <- paste0(trackr_dir, '/', file_hash, '.json')
  
  trackr_json <- jsonlite::toJSON(hashes)
  write(trackr_json, file = trackr_fn)
  
  print(paste0('Successfully written trackr file ', tail(str_split(trackr_fn, '/')[[1]], 1)))
  
  return(NULL)
  
}