#' trackr_history
#' 
#' @description Get the parent records of a trackr_id.
#' 
#' @seealso trackr_lineage
#'
#' @param lineage_file A file path, path to lineage file for a certain trackr_id
#' @param trackr_dir A string, path to store trackr log files.
#' @param return_records A string, return "any" available logged records or require data logs for "all" timepoints.
#' 
#' @importFrom dplyr mutate filter pull
#' @importFrom jsonlite fromJSON
#' 
#' @return A list with the parent parent records for a given trackr_id.
#' @export

trackr_history <- function(lineage_file, trackr_dir, return_records='all'){
  if(!return_records %in% c('any', 'all')){stop('Unknown return_records areguments. Accepts "any", "all".')}
  
  trackr_sum <- trackr_summary(lineage_file)
  
  trackr_sum <- trackr_sum %>% 
          dplyr::mutate(parent_fn = paste0(trackr_dir, '/', lapply(str_split(name, '_'), head, n = 1) %>% unlist(), '_dl.json'),
                        parent_fn_exists = file.exists(parent_fn))
  
  if(return_records == 'all' & sum(!trackr_sum %>% dplyr::pull(parent_fn_exists)) != 0){
    stop('Not all timepoints have a corresponding data log. Use return_records "any" to return any available records')
  }
  
  trackr_sum <- trackr_sum %>% filter(parent_fn_exists)
  
  record_history <- list()
  
  for (i in 1:length(trackr_sum %>% dplyr::pull(1))){
    fn <- c(trackr_sum %>% dplyr::pull(parent_fn))[i]
    
    trackr_fn <- gsub('_dl.json', '.json', fn)
    
    trackr_file <- jsonlite::fromJSON(trackr_fn)
    tstamp <- trackr_file$timestamp
    timepoint_message <- trackr_file$timepoint_message
    
    parent_id <- c(trackr_sum %>% dplyr::pull(children))[i]
    
    json_data <- jsonlite::fromJSON(fn)
    json_data <- json_data %>% dplyr::filter(grepl(parent_id, trackr_id))
    
    record_history[[i]] <- list(timestamp = tstamp, timepoint_message = timepoint_message, json_data)
    
  }
  
  return(record_history)
  
}