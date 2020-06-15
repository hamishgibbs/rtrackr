#' trackr_history
#' 
#' @description Get the parent records of a trackr_id.
#' 
#' @seealso trackr_lineage
#'
#' @param trackr_id A string, the trackr_id used as a starting point for extracting the record lineage.
#' @param trackr_dir A string, path to trackr log files.
#' @param return_records A string (optional), whether to require data log files for all processing steps "all", or any that are available "any".
#' 
#' @importFrom dplyr mutate filter pull
#' 
#' @return A list with the parent parent records for a given trackr_id.
#' @export

trackr_history <- function(trackr_id, trackr_dir, return_records='all'){
  if(!return_records %in% c('any', 'all')){stop('Unknown return_records areguments. Accepts "any", "all".')}
  
  lineage <- trackr_lineage(trackr_id, trackr_dir) %>% 
    filter(hash != "None") %>% 
    dplyr::mutate(parent_file_hash = lapply(stringr::str_split(parent_file, '[.]'), head, n = 1),
                  parent_file_dl_fn = paste0(trackr_dir, '/', parent_file_hash, '_dl.json'),
                  dl_file_exists = lapply(parent_file_dl_fn, file.exists) %>% unlist(),
                  trackr_id = paste0(parent_file_hash, '_', hash))

  root_fn <- lineage %>% slice(length(lineage %>% dplyr::pull(1))) %>% dplyr::pull(parent_file_dl_fn)
  
  #for duplicated records - repeat the previoud file hash appropriately
  dl_files <- lineage %>% arrange(n) %>% dplyr::group_by(parent_file_dl_fn, n) %>% 
    dplyr::summarise(number = n(), .groups = 'drop') %>% 
    arrange(n) %>% 
    dplyr::mutate(parent_file_dl_fn = c('None', head(parent_file_dl_fn, length(parent_file_dl_fn) - 1))) %>% 
    mutate(n_duplicates = lapply(lapply(number, rep, x = "a"), paste, collapse = '/') %>% unlist()) %>% 
    separate_rows(n_duplicates, sep = '/') %>% 
    dplyr::select(-number, -n_duplicates) %>% 
    pull(parent_file_dl_fn)
  
  lineage <- lineage %>% 
    dplyr::mutate(parent_file_dl_fn = dl_files)

  if(return_records == 'all' && length(lineage %>% dplyr::pull(1)) > sum(lineage %>% dplyr::pull(dl_file_exists))){
    stop('Some data log files are missing. Use return_records = "any" to extract any availble data logs.')
  }
  
  record_history <- list()
  
  for (i in 2:(length(lineage %>% dplyr::pull(1)))){
    record <- lineage %>% dplyr::slice(i) %>% tidyr::as_tibble()
    
    parent_file_hash <- record %>% dplyr::pull(parent_file_hash)
    dl_fn <- record %>% dplyr::pull(parent_file_dl_fn)
    
    prev_trackr_id <- record %>% dplyr::mutate(trackr_id = paste0(parent_file_hash, '_', hash)) %>% pull(trackr_id)
    
    if(file.exists(dl_fn)){
      dl <- jsonlite::fromJSON(dl_fn)
      
      dl <- dl %>% dplyr::filter(trackr_id == prev_trackr_id)
      
      record_history[[i - 1]] = list(timepoint_message = record$timepoint_message, timestamp = record$timestamp, data = dl)
      
    }
    
  }
  
  #then - go into the first trackr file and extract 
  dl <- jsonlite::fromJSON(root_fn)
  
  dl <- dl %>% dplyr::filter(trackr_id == prev_trackr_id)
  
  record <- jsonlite::fromJSON(gsub('_dl', '', root_fn))
  
  record_history[[i]] = list(timepoint_message = record$timepoint_message, timestamp = record$timestamp, data = dl)
  
  return(record_history)
  
  
}