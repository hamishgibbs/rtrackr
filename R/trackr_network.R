#' trackr_network
#' 
#' @description Create a simple network plot for a given trackr_lineage file. Requires networkD3.
#'
#' @param lineage_file path, path to lineage file for a certain trackr_id
#' @param ... arguments passed to networkD3::simpleNetwork
#' 
#'
#' @importFrom dplyr as_tibble mutate rename select filter pull
#' @importFrom tidyr drop_na separate_rows
#' 
#' @return networkD3 network graph
#' @export

trackr_network <- function(lineage_file, ...){
  #simple network option - truncate trackr_id
  
  d <- jsonlite::fromJSON(lineage_file)
  
  
  d <- suppressWarnings({d[c('name', 'children')] %>% 
    tidyr::separate_rows('children', sep = ', ') %>% 
    dplyr::mutate(children = gsub('c[(]', '', children),
           children = gsub('[)]', '', children),
           children = gsub('[()]', '', children),
           children = gsub('"', '', children),
           children = gsub('list()', '', children),
           children = ifelse(children == '', NA, children),
           name = unlist(name)) %>% 
    tidyr::drop_na(children)})
  

  if (requireNamespace("networkD3", quietly=TRUE)) {
    networkD3::simpleNetwork(d, ...)
  }else{
    stop('Package "networkD3" is required for trackr_network')
  }

}