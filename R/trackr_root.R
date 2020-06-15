#' trackr_root
#' 
#' @description Get the root record of a trackr_id.
#' 
#' @seealso trackr_lineage trackr_history
#'
#' @param trackr_id A string, the trackr_id used as a starting point for extracting the record lineage.
#' @param trackr_dir A string, path to trackr log files.
#' 
#' @return A list, the root record for a given trackr_id.
#' @export

trackr_root <- function(trackr_id, trackr_dir){
  
  hist <- trackr_history(trackr_id, trackr_dir)
  
  return(hist[[length(hist)]])
  
}