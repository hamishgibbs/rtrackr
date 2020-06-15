get_parent_file_timestamp <- function(parent_file){
  return(jsonlite::fromJSON(parent_file)$timestamp)
}
