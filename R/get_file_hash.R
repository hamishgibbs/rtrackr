# get_file_hash
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
#
# @importFrom digest sha1
# 
# @return character, hash of current file row hashes

get_file_hash <- function(hash_string){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  file_hash <- digest::sha1(paste(hash_string$hash, collapse = ''))
  
  return(file_hash)
  
}