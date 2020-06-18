#' trackr_network
#' 
#' @description Create a simple network plot for a given trackr_lineage file. Requires networkD3.
#'
#' @param lineage_file path, path to lineage file for a certain trackr_id
#' @param truncate_hashes A boolean, whether to shorten hashes for display.
#' @param ... arguments passed to networkD3::simpleNetwork
#' 
#'
#' @importFrom dplyr mutate
#' @importFrom stringr str_split
#' 
#' @return networkD3 network graph
#' @export

trackr_network <- function(lineage_file, truncate_hashes = FALSE, ...){
  #simple network option - truncate trackr_id
  
  d <- trackr_summary(lineage_file)
  
  if(truncate_hashes){
    d <- d %>% dplyr::mutate(name = sapply(name, truncate_id),
                 children = sapply(children, truncate_id))
  }
  
  if (requireNamespace("networkD3", quietly=TRUE)) {
    networkD3::simpleNetwork(d, ...)
  }else{
    stop('The package "networkD3" is required for trackr_network')
  }

}

truncate_id <- function(id){
  
  ssplit <- stringr::str_split(id, '_')[[1]]
  pfh <- substr(ssplit[1], 1, 5)
  rh <- substr(ssplit[2], 1, 5)
  
  return(paste0(pfh, '_', rh))
  
}
