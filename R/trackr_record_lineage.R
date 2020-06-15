#' trackr_record_lineage
#' 
#' @description Get the lineage of a trackr_id
#'
#' @param trackr_id string trackr_id used as starting point for extracting lineage
#' @param trackr_dir path, path of trackr log files
#' @param return_class optional character, return a data.frame or list, default data.frame
#' 
#' @importFrom dplyr mutate filter pull
#' 
#' @return data.frame or list with lineage for one trackr_id
#' @export

trackr_record_lineage <- function(trackr_id, trackr_dir, return_class = "data.frame"){
  if(!return_class %in% c('data.frame', 'list')){stop('Unknown return class. Accepts "data.frame", "list".')}
  if(length(trackr_id) > 1){stop('trackr_record_lineage only accepts one trackr id.')}
  
  #accept a trackr id - not a hash - string parent file hash from trackr_id
  #altering data structure to play with extract_parent_file_hash
  trackr_id <- data.frame(trackr_old_hash = trackr_id)
  
  parent_file_hash <- extract_parent_file_hash(trackr_id)
  
  trackr_id <- remove_parent_file_hash(trackr_id, parent_file_hash)
  
  target_hash <- trackr_id$trackr_old_hash
  
  trackr_sum <- trackr_summary(trackr_dir) %>% 
    dplyr::mutate(timepoint_message = as.character(timepoint_message))
  
  if(!"root" %in% c(trackr_sum %>% pull(type) %>% unique())){
    stop('No root nodes found.')
  }
  
  #favor the most recent record
  tar_time <- trackr_sum %>% dplyr::filter(hash %in% target_hash) %>% dplyr::pull(timestamp) %>% max()
  new_hash <- trackr_sum %>% dplyr::filter(hash == target_hash & timestamp == tar_time) %>% dplyr::pull(parent_hash)
  
  record_lineage <- list()
  timepoint <- 1
  while (new_hash[1] != 'None') {
    
    sum_f <- trackr_sum %>% dplyr::filter(hash %in% new_hash & timestamp < tar_time) %>% dplyr::filter(timestamp == max(timestamp))
    tar_time <- sum_f %>% dplyr::pull(timestamp) %>% max()
    new_hash <- sum_f %>% dplyr::pull(parent_hash) %>% unique()
    timepoint_message <- sum_f %>% dplyr::pull(timepoint_message) %>% unique()
    
    record_lineage[[timepoint]] = list(hash = new_hash, timestamp = tar_time, timepoint_message = timepoint_message)
    timepoint <- timepoint + 1
  }
  
  if(return_class == "data.frame"){
    return(do.call(rbind, lapply(record_lineage, data.frame, stringsAsFactors = F)))
  }else(
    return(record_lineage)
  )
  
}
