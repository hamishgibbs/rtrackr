#' trackr_timepoint
#' 
#' @description Log a data processing timepoint and write a trackr file log changes
#'
#' @param dataframe data.frame, data to be logged
#' @param trackr_dir path, path of trackr log files
#' @param timepoint_message optional character, message to identify timepoint - similar to a git commit message
#'
#' @importFrom dplyr mutate rename pull distinct
#' @importFrom tidyr separate_rows
#' @importFrom jsonlite fromJSON
#' 
#' @return data.frame with updated trackr_id column
#' @export

trackr_timepoint <- function(dataframe, trackr_dir = '~/Documents/Personal/trackr_dev/trackr_dir', timepoint_message = NULL){
  if (!is.data.frame(dataframe)){stop("dataframe must be a data.frame")}
  
  if ('trackr_old_hash' %in% colnames(dataframe)){stop('trackr_old_hash is used by trackr internally - please rename trackr_old_hash column')}
  
  if (!'trackr_id' %in% colnames(dataframe)){stop('dataframe does not contain a "trackr_id" column')}
  
  #check for duplicate tracker ids - from a summarise operation
  if(sum(str_detect(dataframe$trackr_id, ', ')) > 0){
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
  
  json_data <- jsonlite::fromJSON(trackr_parent_fn)
  
  #check if any records are missing - for now this is hard coded until we can adjust to summarising data 
  #- it would be ok to just drop data 
  if(length(json_data[['trackr_ids']]) > hash_string %>% dplyr::pull(1) %>% length()){
    stop("Some records are missing from the original dataset. This is not currently supported.")
  }
  
  file_hash <- get_file_hash(hash_string)
  
  write_timepoint_trackr_file(hash_string, parent_file_hash, file_hash, trackr_dir, timepoint_message)
  
  #trackr_id is file_hash + '_' + record_hash
  dataframe <- dataframe %>% dplyr::mutate(trackr_id = paste0(file_hash, '_', hash_string$hash))
  
  #for records that have been summarised - the opposite of the separate_rows operation
  dataframe <- dplyr::distinct(dataframe)
  
  return(dataframe)
  
  
}
