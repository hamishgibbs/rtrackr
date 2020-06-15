#' verify_data_log
#' 
#' @description Verify a data log file. The data log file is the hash of all row hashes.
#'
#' @param log_file A string, the path to a log file
#'
#' @importFrom dplyr rename
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_split
#' 
#' @return A boolean, whether the log file has been validated successfully (TRUE/FALSE)
#' @export

verify_data_log <- function(log_file){
  
  if(!file.exists(log_file)){
    stop('Log file not found.')
  }
  
  log_hash <- substr(tail(stringr::str_split(log_file, '/')[[1]], 1), 1, 40)
  
  logged_data <- jsonlite::fromJSON(log_file) %>% dplyr::as_tibble()
  
  hash_string <- trackr_hash(logged_data %>% dplyr::rename(trackr_old_hash = trackr_id))
  
  print(hash_string)
  
  file_hash <- get_file_hash(hash_string)
  
  print(log_hash)
  print(file_hash)
  
  return(log_hash == file_hash)
  
}

