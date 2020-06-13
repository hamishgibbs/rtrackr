#' verify_data_log
#' 
#' @description Log a data processing timepoint
#'
#' @param log_file character path to log file
#'
#' @importFrom dplyr rename
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_split
#' 
#' @return boolean, log file validated (Y/N)
#' @export

verify_data_log <- function(log_file){
  
  log_hash <- substr(tail(stringr::str_split(log_file, '/')[[1]], 1), 1, 40)
  
  logged_data <- jsonlite::fromJSON(log_file) %>% dplyr::as_tibble()
  
  hash_string <- trackr_hash(logged_data %>% dplyr::rename(trackr_old_hash = trackr_id))
  
  file_hash <- get_file_hash(hash_string)
  
  return(log_hash == file_hash)
  
}

