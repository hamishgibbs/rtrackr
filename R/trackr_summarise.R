#' trackr_summarise
#' 
#' @description Wrapper for dplyr::summarise to summarise data and retain trackr_ids
#'
#' @param dataframe data.frame, data to summarised
#' @param ... arguments passed to dplyr::summarise
#'
#' @description Summarise data with dplyr::summarise and retain trackr_ids
#' 
#' @return A summarised dataframe
#' @export
#' @importFrom dplyr summarise left_join

trackr_summarise <- function(dataframe, ...){
  
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  if (is.null(groups(dataframe))){stop("dataframe must be grouped")}
  
  group_id_assigned <- dataframe %>% dplyr::summarise(trackr_id = paste(trackr_id, collapse = ', '))
  
  dataframe <- dataframe %>% dplyr::summarise(...)
  
  #After summarising, reattach collapsed trackr_ids
  dataframe <- dataframe %>% 
    dplyr::left_join(group_id_assigned, by = lapply(groups(t), deparse) %>% unlist())
  
  return(dataframe)
}
