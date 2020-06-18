#' trackr_summary
#' 
#' @description Process the lineage (parent trackr_ids) of a trackr_id.
#'
#' @param lineage_file path, path to lineage file for a certain trackr_id
#' 
#' @importFrom dplyr mutate filter pull
#' @importFrom jsonlite toJSON
#' 
#' @return A dataframe, a summary of a trackr lineage file.
#' @export

trackr_summary <- function(lineage_file){
  
  if(!file.exists(lineage_file)){
    stop('File not found.')
  }
  
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
  
  return(d)
  
}
