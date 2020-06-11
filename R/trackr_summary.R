#' trackr_summary
#' 
#' @description Make a summary file from a directory of trackr history files
#' 
#' @param tracker_dir 
#'
#' @return
#' @export
#'
#' @examples

trackr_summary <- function(tracker_dir){
  files <- list.files(path = tracker_dir, pattern = '.json', full.names = T)
  
  trackr_history <- lapply(files, jsonlite::fromJSON)
  
  metadata <- suppressWarnings(do.call(bind_rows, lapply(trackr_history, data.frame))) %>% as_tibble()
  
  trackr_ids <- do.call(bind_rows, lapply(trackr_history, `[[`, 'trackr_ids'))
  
  
  metadata <- metadata %>% dplyr::select(-trackr_ids.type, -trackr_ids.hash, -trackr_ids.parent_hash)
  
  df <- cbind(metadata, trackr_ids)
  
  df <- df %>% mutate_at(c('type', 'hash', 'parent_hash'), unlist)
  
  df <- df %>% mutate(timestamp = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))
  
  return(df)
  
}


