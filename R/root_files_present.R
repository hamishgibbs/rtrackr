root_files_present <- function(trackr_dir){
  return(list.files(path = trackr_dir, pattern = 'root.json') %>% length() > 0)
}
