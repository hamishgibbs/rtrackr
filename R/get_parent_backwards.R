# trace_backwards
#
# @description get the parent id of a given trackr_id
#
# @param trackr_id string, a trackr_id
# 
# @return character vector, of parent id(s)

get_parent_backwards <- function(trackr_id){
  #get previous parent(s)
  
  start_file <- paste0(trackr_dir, '/',c(lapply(str_split(trackr_id, '_'), head, n = 1) %>% unlist()), '.json')
  
  trackr_file <- jsonlite::fromJSON(start_file)
  
  parent_id <- trackr_file$trackr_ids$parent_id[which(trackr_file$trackr_ids$id == trackr_id)] %>% unlist()
  
  return(parent_id)
  
}