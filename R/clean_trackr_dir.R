#' clean_trackr_dir
#' 
#' @description Remove files in the trackr_dir
#'
#' @param trackr_dir path, path of trackr log files
#'
#' @importFrom stringr str_split
#' 
#' @export
 
clean_trackr_dir <- function(trackr_dir){
  
  trackr_files <- list.files(path=trackr_dir, pattern=".json", full.names = T)
  
  if (length(trackr_files) > 0){
    
    file.remove(trackr_files) 
    
    print(paste0('Removed: ', lapply(stringr::str_split(trackr_files, '/'), tail, n = 1) %>% unlist())) 
    
  }else{
    stop('No trackr files to remove.')
  }
  
}