#' trackr_lineage
#' 
#' @description Get the lineage (parent trackr_ids) of a trackr_id and store it in the trackr_dir.
#'
#' @param trackr_id A string, the trackr_id used as a starting point for extracting the record lineage.
#' @param trackr_dir A string, path to trackr log files.
#' 
#' @importFrom dplyr mutate filter pull
#' @importFrom jsonlite toJSON
#' 
#' @export

trackr_lineage <- function(trackr_id, trackr_dir){
  
  tree <- trace_backwards(trackr_id)
  end_node <- tree[[length(tree)]]$children
  nodes_to_trace <- c(end_node)
  results <- list()
  results[[1]] <- tree
  i = 2
  while(length(nodes_to_trace) > 0){
    result <- trace_backwards(nodes_to_trace[1])
    results[[i]] <- result
    
    #append last children to nodes to trace
    end_node <- result[[length(result)]]$children
    
    nodes_to_trace <- append(nodes_to_trace, end_node)
    nodes_to_trace <- nodes_to_trace[!nodes_to_trace %in% nodes_to_trace[1]]
    
    i = i + 1
    
  }
  
  #back the nesting out one level
  history <- jsonlite::toJSON(unlist(results,recursive=FALSE))
  
  history_fn <- paste0(trackr_id, '_lineage.json')
  
  write(history, paste0(trackr_dir, '/', history_fn))
  
  print(paste0('Successfully written ', history_fn))
  
}
