#' validate_data_log
#' 
#' @description Validate a data log file. The data log file is the hash of all row hashes.
#'
#' @param log_file A string, the path to a log file
#'
#' @importFrom dplyr rename
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_split
#' 
#' @return A boolean, whether the log file has been validated successfully (TRUE/FALSE)
#' @export

validate_data_log <- function(log_file){
  
  if(!file.exists(log_file)){
    stop('Log file not found.')
  }
  
  log_hash <- substr(tail(stringr::str_split(log_file, '/')[[1]], 1), 1, 40)
  
  log_tstamp <- get_parent_file_timestamp(gsub('_dl.json', '.json', log_file))
  
  logged_data <- jsonlite::fromJSON(log_file)
  
  file_hash <- get_file_hash(logged_data, log_tstamp)
  
  return(log_hash == file_hash)
  
}

