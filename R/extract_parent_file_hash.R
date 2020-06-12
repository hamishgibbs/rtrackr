# extract_parent_file_hash
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
#
# @return character hash of parent file row hashes (first component of trackr_id)

extract_parent_file_hash <- function(hash_string){
  parent_file_hash <- lapply(str_split(hash_string$trackr_old_hash, '_'), head, n = 1) %>% unlist() %>% unique()
  
  if(length(parent_file_hash) > 1){
    stop('Multiple parent files in the input dataframe')
  }
  
  return(parent_file_hash)
  
}