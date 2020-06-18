get_parent_file_timestamp <- function(parent_file){
  
  tstamps <- lapply(parent_file, jsonlite::fromJSON)
  
  tstamp <- lapply(tstamps, `[[`, 'timestamp') %>% unlist() %>% max()
  
  return(tstamp)
  
}