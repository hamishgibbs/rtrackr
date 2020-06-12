# get_file_hash
#
# @param dataframe data.frame, data to be given trackr_ids
#
# @importFrom digest sha1
# @importFrom tidyr unite
# @importFrom dplyr mutate
# 
# @return character, hash of current file row hashes

trackr_hash <- function(dataframe){
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  hash_string <- dataframe %>% tidyr::unite(col = "hash_string", colnames(dataframe)[!colnames(dataframe) %in% c('trackr_id', 'trackr_old_hash')], sep = '')
  
  hash_string <- hash_string %>% dplyr::mutate(hash = lapply(hash_string, digest::sha1) %>% unlist())
  
  return(hash_string)
}