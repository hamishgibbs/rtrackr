# remove_parent_file_hash
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
# @param parent_file_hash character, hash of parent file row hashes
#
# @importFrom dplyr mutate
# 
# @return data.frame hash string with parent_file_hash removed from trackr_old_hash

remove_parent_file_hash <-  function(hash_string, parent_file_hash){
  hash_string <- hash_string %>% 
    dplyr::mutate(trackr_old_hash = gsub(paste0(!!parent_file_hash, '_'), '', trackr_old_hash))
  
  return(hash_string)
}