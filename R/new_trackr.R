#' new_trackr
#'
#' @param dataframe data.frame, data to be added to trackr
#'
#' @importFrom tidyr unite
#' @importFrom dplyr mutate
#' @importFrom digest sha1
#' 
#' @return trackr
#' @export
#'
#' @examples


new_trackr <- function(dataframe, trackr_dir = '~/Documents/Personal/trackr/trackr_dir'){
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  hash_string <- trackr_hash(dataframe)
  
  if(!c(hash_string$hash %>% unique() %>% length()) == c(hash_string$hash %>% length())){
    stop('Duplicate rows detected. Unable to create unique record ids.')
  }
  
  file_hash <- get_file_hash(hash_string)
  #write reference file here - could be a function
  write_new_trackr_file(hash_string, file_hash, trackr_dir)
  
  #trackr_id is file_hash + '_' + record_hash
  dataframe <- dataframe %>% dplyr::mutate(trackr_id = paste0(file_hash, '_', hash_string$hash))
  
  return(dataframe)
  
}

write_new_trackr_file <- function(hash_string, file_hash, trackr_dir){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  tstamp <- as.numeric(Sys.time())
  hashes <- list()
  
  for(i in 1:length(hash_string %>% pull(hash))){
    
    hash <- hash_string$hash[i]
    
    hashes[[i]] = list(type = 'root', hash = hash)
    
  }
  
  hashes = list(parent_file = 'None', timestamp = tstamp, trackr_ids = hashes)
  trackr_fn <- paste0(trackr_dir, '/', file_hash, '.json')
  trackr_json <- jsonlite::toJSON(hashes)
  write(trackr_json, file = trackr_fn)
  
  print(paste0('Successfully written trackr file ', tail(str_split(trackr_fn, '/')[[1]], 1)))
  
  return(NULL)
  
}

trackr_hash <- function(dataframe){
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  hash_string <- dataframe %>% tidyr::unite(col = "hash_string", colnames(dataframe)[!colnames(dataframe) %in% c('trackr_id', 'trackr_old_hash')], sep = '')

  hash_string <- hash_string %>% dplyr::mutate(hash = lapply(hash_string, digest::sha1) %>% unlist())
  
  return(hash_string)
}

get_file_hash <- function(hash_string){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  file_hash <- digest::sha1(paste(hash_string$hash, collapse = ''))
  
  return(file_hash)
  
}

trackr_timepoint <- function(dataframe, trackr_dir = '~/Documents/Personal/trackr/trackr_dir', timepoint_message = NULL){
  if (!is.data.frame(dataframe)){stop("dataframe must be a data.frame")}
  
  if ('trackr_old_hash' %in% colnames(dataframe)){stop('trackr_old_hash is used by trackr internally - please rename trackr_old_hash column')}
  
  if (!'trackr_id' %in% colnames(dataframe)){stop('dataframe does not contain a "trackr_id" column')}
  
  #check for duplicate tracker ids - from a summarise operation
  if(sum(str_detect(dataframe$trackr_id, ', ')) > 0){
    dataframe <- dataframe %>% separate_rows(trackr_id, sep = ', ')
  }
  
  hash_string <- trackr_hash(dataframe %>% rename(trackr_old_hash = trackr_id))
  
  parent_file_hash <- extract_parent_file_hash(hash_string)

  hash_string <- remove_parent_file_hash(hash_string, parent_file_hash)
  
  #check the number of changes is > 0
  if(hash_string %>% mutate(changed = trackr_old_hash == hash) %>% pull(changed) %>% sum() == hash_string %>% pull(1) %>% length()){
    stop('No changes detected. Exiting.')
  }

  #get the previous log file with file_hash
  trackr_parent_fn <- paste0(trackr_dir, '/', parent_file_hash, '.json')
  
  json_data <- jsonlite::fromJSON(trackr_parent_fn)
  
  #check if any records are missing - for now this is hard coded until we can adjust to summarising data 
  #- it would be ok to just drop data 
  if(length(json_data[['trackr_ids']]) > hash_string %>% pull(1) %>% length()){
    stop("Some records are missing from the original dataset. This is not currently supported.")
  }
  
  file_hash <- get_file_hash(hash_string)
  
  write_timepoint_trackr_file(hash_string, parent_file_hash, file_hash, trackr_dir, timepoint_message)
  
  #trackr_id is file_hash + '_' + record_hash
  dataframe <- dataframe %>% dplyr::mutate(trackr_id = paste0(file_hash, '_', hash_string$hash))
  
  #for records that have been summarised - the opposite of separate_rows operation
  dataframe <- distinct(dataframe)
  
  return(dataframe)
  
  
}

extract_parent_file_hash <- function(hash_string){
  parent_file_hash <- lapply(str_split(hash_string$trackr_old_hash, '_'), head, n = 1) %>% unlist() %>% unique()
  
  if(length(parent_file_hash) > 1){
    stop('Multiple parent files in the input dataframe')
  }
  
  return(parent_file_hash)
  
}

remove_parent_file_hash <-  function(hash_string, parent_file_hash){
  hash_string <- hash_string %>% 
    mutate(trackr_old_hash = gsub(paste0(!!parent_file_hash, '_'), '', trackr_old_hash))
  
  return(hash_string)
}

write_timepoint_trackr_file <- function(hash_string, parent_file_hash, file_hash, trackr_dir, timepoint_message){
  if (!is.data.frame(hash_string)) stop("hash_string must be a data.frame")
  
  tstamp <- as.numeric(Sys.time())
  hashes <- list()
  
  #can't have duplicate list indices
  for(i in 1:length(hash_string %>% pull(hash))){
    hash <- hash_string$hash[i]
    trackr_old_hash <- hash_string$trackr_old_hash[i]
    
    hashes[[i]] = list(type = 'node', hash = hash, parent_hash = trackr_old_hash)
    
  }
  
  hashes = list(parent_file = paste0(parent_file_hash, '.json'), timestamp = tstamp, timepoint_message = timepoint_message, trackr_ids = hashes)
  trackr_fn <- paste0(trackr_dir, '/', file_hash, '.json')
  trackr_json <- jsonlite::toJSON(hashes)
  write(trackr_json, file = trackr_fn)
  
  print(paste0('Successfully written trackr file ', tail(str_split(trackr_fn, '/')[[1]], 1)))
  
}

trackr_summarise <- function(dataframe, ...){
  #wrapper for dplyr::summarise to retain all tracker_id information
  
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  if (is.null(groups(dataframe))){stop("dataframe must be grouped")}
  
  group_id_assigned <- dataframe %>% summarise(trackr_id = paste(trackr_id, collapse = ', '))
  
  dataframe <- dataframe %>% summarise(...)
  
  #After summarising, reattach collapsed tracker ids
  dataframe <- dataframe %>% 
    left_join(group_id_assigned, by = lapply(groups(t), deparse) %>% unlist())
  
  return(dataframe)
}

#combine_trackr function

#code_cov - public - travis - and documentation "how it works"

#trackr_summary(trackr_path) 
#on docs site - website to upload and parse a trackr summary file - network graph & search function



