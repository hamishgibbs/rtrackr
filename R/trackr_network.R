#' trackr_network
#' 
#' @description Create a simple network plot from a collectino of trackr log files. Requires networkD3.
#'
#' @param trackr_dir path, path to a directory of trackr log files
#' @param ... arguments passed to networkD3::simpleNetwork
#' 
#'
#' @importFrom dplyr as_tibble mutate rename select filter pull
#' 
#' @return networkD3 network graph
#' @export

trackr_network <- function(trackr_dir, ...){
  
  his <- trackr_summary(trackr_dir) %>% dplyr::as_tibble()
  
  his <- his %>% dplyr::rename(target = hash, src = parent_hash) %>% 
    dplyr::select(src, target) %>% 
    dplyr::filter(src != 'None')
  
  uniq <- unique(c(his %>% dplyr::pull(src) %>% unique(), his %>% dplyr::pull(target) %>% unique()))
  src <- his %>% dplyr::pull(src)
  target <- his %>% dplyr::pull(target)
  ids <- as.character(1:length(uniq))
  
  for(i in 1:length(uniq)){
    src <- gsub(uniq[i], ids[i], src)
    target <- gsub(uniq[i], ids[i], target)
  }
  
  his$src <- src
  his$target <- target
  
  if (requireNamespace("networkD3", quietly=TRUE)) {
    networkD3::simpleNetwork(his, ...)
  }else{
    stop('Package "networkD3" is required for trackr_network')
  }

}