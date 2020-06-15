#' trackr_lineage
#' 
#' @description Get the lineage (parent hashes) of a trackr_id.
#' 
#' @seealso trackr_history
#'
#' @param trackr_id A string, the trackr_id used as a starting point for extracting the record lineage.
#' @param trackr_dir A string, path to trackr log files.
#' @param return_class A string (optional), format of output data, a "data.frame" or "list". Default is "data.frame".
#' 
#' @importFrom dplyr mutate filter pull
#' 
#' @return A data.frame or list with the lineage (parent hashes) of all parent records for a given trackr_id.
#' @export

trackr_lineage <- function(trackr_id, trackr_dir, return_class = "data.frame"){
  if(!return_class %in% c('data.frame', 'list')){stop('Unknown return class. Accepts "data.frame", "list".')}
  if(length(trackr_id) > 1){stop('trackr_lineage only accepts one trackr id.')}
  
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
  record_lineage <- list()
  
  record_lineage[[1]] = list(n = 1, hash = target_hash, timestamp = Inf, timepoint_message = "Target record", parent_file = paste0(parent_file_hash, '.json'))
  
  tar_time <- trackr_sum %>% dplyr::filter(hash %in% target_hash) %>% dplyr::pull(timestamp) %>% max()
  sum_f <- trackr_sum %>% dplyr::filter(hash == target_hash & timestamp == tar_time)
  new_hash <- sum_f %>% dplyr::pull(parent_hash)
  timepoint_message <- sum_f %>% dplyr::pull(timepoint_message) %>% unique()
  parent_file <- sum_f %>% dplyr::pull(parent_file) %>% unique()
  record_lineage[[2]] = list(n = 2, hash = new_hash, timestamp = tar_time, timepoint_message = timepoint_message, parent_file = parent_file)
  
  
  timepoint <- 3
  while (new_hash[1] != 'None') {
    
    sum_f <- trackr_sum %>% dplyr::filter(hash %in% new_hash & timestamp < tar_time) %>% dplyr::filter(timestamp == max(timestamp))
    tar_time <- sum_f %>% dplyr::pull(timestamp) %>% max()
    new_hash <- sum_f %>% dplyr::pull(parent_hash) %>% unique()
    timepoint_message <- sum_f %>% dplyr::pull(timepoint_message) %>% unique()
    parent_file <- sum_f %>% dplyr::pull(parent_file) %>% unique()
    
    record_lineage[[timepoint]] = list(n = timepoint, hash = new_hash, timestamp = tar_time, timepoint_message = timepoint_message, parent_file = parent_file)
    timepoint <- timepoint + 1
  }
  
  if(return_class == "data.frame"){
    return(do.call(rbind, lapply(record_lineage, data.frame, stringsAsFactors = F)))
  }else(
    return(record_lineage)
  )
  
}
