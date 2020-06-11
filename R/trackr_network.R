trackr_network <- function(tracker_dir, ...){
  require(networkD3)
  
  his <- trackr_summary(tracker_dir) %>% as_tibble()
  
  his <- his %>% rename(target = hash, src = parent_hash) %>% 
    select(src, target) %>% 
    filter(src != 'None')
  
  uniq <- unique(c(his %>% pull(src) %>% unique(), his %>% pull(target) %>% unique()))
  src <- his %>% pull(src)
  target <- his %>% pull(target)
  ids <- as.character(1:length(uniq))
  
  for(i in 1:length(uniq)){
    src <- gsub(uniq[i], ids[i], src)
    target <- gsub(uniq[i], ids[i], target)
  }
  
  his$src <- src
  his$target <- target
  
  simpleNetwork(his, ...)
  
}