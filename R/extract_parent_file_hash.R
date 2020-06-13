# extract_parent_file_hash
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
#
# @importFrom stringr str_split
#
# @return character hash of parent file row hashes (first component of trackr_id)

extract_parent_file_hash <- function(hash_string){

  parent_file_hash <- lapply(stringr::str_split(hash_string$trackr_old_hash, '_'), head, n = 1) %>% unlist() %>% unique()
  
  return(parent_file_hash)
  
}