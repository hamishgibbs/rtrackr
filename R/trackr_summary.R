#' trackr_summary
#' 
#' @description Create a summary data.frame from a directory of trackr log files
#'
#' @param trackr_dir path, path to a directory of trackr log files
#'
#' @importFrom dplyr bind_rows as_tibble select mutate_at mutate
#' @importFrom jsonlite fromJSON
#' 
#' @return data.frame summary of trackr log files
#' @export

trackr_summary <- function(trackr_dir){
  files <- list.files(path = trackr_dir, pattern = '.json', full.names = T)
  
  trackr_history <- lapply(files, jsonlite::fromJSON)
  
  metadata <- suppressWarnings(do.call(dplyr::bind_rows, lapply(trackr_history, data.frame))) %>% dplyr::as_tibble()
  
  trackr_ids <- do.call(dplyr::bind_rows, lapply(trackr_history, `[[`, 'trackr_ids'))
  
  
  metadata <- metadata %>% dplyr::select(-trackr_ids.type, -trackr_ids.hash, -trackr_ids.parent_hash)
  
  df <- cbind(metadata, trackr_ids)
  
  df <- df %>% dplyr::mutate_at(c('type', 'hash', 'parent_hash'), unlist)
  
  df <- df %>% dplyr::mutate(timestamp = as.Date(as.POSIXct(timestamp, origin="1970-01-01")))
  
  return(df)
  
}


