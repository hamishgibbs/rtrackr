# get_file_hash
#
# @param hash_string data.frame, dataframe of old and new hashes with colnames hash and trackr_old_hash
#
# @importFrom digest sha1
# 
# @return character, hash of current file row hashes

get_file_hash <- function(dataframe, tstamp){
  if (!is.data.frame(dataframe)) stop("hash_string must be a data.frame")
  
  hash_ref <- dataframe %>% tidyr::unite(col = "hash_string", colnames(dataframe)[!colnames(dataframe) %in% c('trackr_id', 'trackr_old_hash')], sep = '')
  
  hash_ref <- hash_ref %>% dplyr::mutate(hash = lapply(hash_ref$hash_string, digest::sha1) %>% unlist())
  
  file_hash <- digest::sha1(paste0(tstamp, paste0(hash_ref$hash, collapse = '')))
  
  if(length(file_hash) > 1){stop('more that one file hash returned')}
  
  return(file_hash)
  
}
