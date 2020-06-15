#' trackr_timepoint
#' 
#' @description Log a timepoint in the data processing chain.
#'
#' @param dataframe A data.frame, the data to be logged.
#' @param trackr_dir A string, path to store trackr log files.
#' @param timepoint_message A string (optional), a message to identify the timepoint - similar to a git commit message.
#' @param log_data A boolean (optional), output a full dataset log with each trackr file. Default is "TRUE"
#' 
#' @importFrom dplyr mutate rename pull distinct
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_detect
#' 
#' @return A data.frame with an updated trackr_id column. Trackr log and data log files are written into the trackr_dir.
#' @export

trackr_timepoint <- function(dataframe, trackr_dir = NULL, timepoint_message = NULL, log_data = TRUE){
  
  if (is.null(trackr_dir)){stop('No trackr_dir specified. Please specify where to store trackr log files.')}
  
  if (!is.data.frame(dataframe)){stop("dataframe must be a data.frame")}
  
  if ('trackr_old_hash' %in% colnames(dataframe)){stop('trackr_old_hash is used by trackr internally - please rename trackr_old_hash column')}
  
  if (!'trackr_id' %in% colnames(dataframe)){stop('dataframe does not contain a "trackr_id" column')}
  
  n_records <- dataframe %>% dplyr::pull(1) %>% length
  
  #check for duplicate trackr ids - from a summarise operation
  if(sum(stringr::str_detect(dataframe$trackr_id, ', ')) > 0){
    dataframe <- dataframe %>% tidyr::separate_rows(trackr_id, sep = ', ')
  }
  
  hash_string <- trackr_hash(dataframe %>% dplyr::rename(trackr_old_hash = trackr_id))
  
  parent_file_hash <- extract_parent_file_hash(hash_string)
  
  hash_string <- remove_parent_file_hash(hash_string, parent_file_hash)
  
  #check the number of changes is > 0
  if(hash_string %>% dplyr::mutate(changed = trackr_old_hash == hash) %>% dplyr::pull(changed) %>% sum() == hash_string %>% dplyr::pull(1) %>% length()){
    stop('No changes detected. Exiting.')
  }
  
  #get the previous log file with file_hash
  trackr_parent_fn <- paste0(trackr_dir, '/', parent_file_hash, '.json')
  
  #if unix timestamp now is <1 second different from parent file - wait a second
  if(as.numeric(Sys.time()) - get_parent_file_timestamp(trackr_parent_fn) < 1){Sys.sleep(1)}
  
  #json_data <- jsonlite::fromJSON(trackr_parent_fn)
  #could check recursively for all history in trackr_dir
  if(all(!sapply(trackr_parent_fn, file.exists))){
    stop('Parent trackr file not found in trackr_dir. Exiting.')
  }
  
  file_hash <- get_file_hash(hash_string)
  
  write_timepoint_trackr_file(hash_string, parent_file_hash, file_hash, trackr_dir, timepoint_message)
  
  if(log_data){
    write_data_log(dataframe, trackr_dir, file_hash)
  }
  
  #trackr_id is file_hash + '_' + record_hash - need to retain unaltered version of records that have not changed and rbind changed records to it. 
  dataframe <- dataframe %>% dplyr::mutate(trackr_id = paste0(file_hash, '_', hash_string$hash))
  
  #for records that have been summarised - the opposite of the separate_rows operation
  dataframe <- dplyr::distinct(dataframe)
  
  if(dataframe %>% dplyr::pull(1) %>% length() != n_records){
    stop('Output is not the same length as input.')
  }
  
  return(dataframe)
  
  
}





