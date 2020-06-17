# write_timepoint_trackr_file
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
# @param parent_file_hash character, hash of parent file row hashes
# @param file_hash character, hash of current file row hashes
# @param trackr_dir path, path to store log file
# @param timepoint_message optional character, message to identify timepoint - similar to a git commit message
#
# @importFrom jsonlite toJSON
# @importFrom stringr str_split

write_timepoint_trackr_file <- function(hash_string, input_dataframe, parent_file_hash, file_hash, trackr_dir, timepoint_message, tstamp){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  hashes <- list()
  
  #can't have duplicate list indices
  for(i in 1:length(hash_string %>% pull(hash))){
    hash <- hash_string$hash[i]
    trackr_old_id <- input_dataframe$trackr_id[i]
    
    hashes[[i]] = list(type = 'node', id = paste0(file_hash, '_', hash), parent_id = trackr_old_id)
    
  }
  
  hashes = list(parent_file = paste0(parent_file_hash, '.json'), timestamp = tstamp, timepoint_message = timepoint_message, trackr_ids = hashes)
  trackr_fn <- paste0(trackr_dir, '/', file_hash, '.json')
  trackr_json <- jsonlite::toJSON(hashes)
  write(trackr_json, file = trackr_fn)
  
  print(paste0('Successfully written trackr file ', tail(stringr::str_split(trackr_fn, '/')[[1]], 1)))
  
}