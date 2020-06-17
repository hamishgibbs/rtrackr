# trace_backwards
#
# @description recursively traverse a log file tree to identify parent nodes of a given trackr_id
#
# @param target_id string, a trackr_id
# 
# @return list, the trackr_ids of parent record(s)

trace_backwards <- function(target_id){
  
  parent_id <- target_id
  
  parents <- list()
  
  i = 1
  while(length(parent_id) == 1){
    prev_id <- parent_id
    parent_id <- get_parent_backwards(parent_id)
    parents[[i]] <- list(name = prev_id, children = parent_id, type = 'node')
    i = i + 1
  }
  
  if(is.null(parents[[i - 1]]$children)){
    parents[[i - 1]]$type = 'root'
    parents <- parents[1:(i - 1)]
  }
  
  if(length(parents[[i - 1]]$children) > 1){
    parents[[i - 1]]$type = 'break_point'
  }
  
  return(parents)
  
}