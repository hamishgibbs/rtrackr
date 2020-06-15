#' trackr_new
#' 
#' @description create a new tracker file, the starting point of a tracking history
#'
#' @param dataframe data.frame, data to be added to trackr
#' @param trackr_dir path, path to store log files
#' @param timepoint_message optional character, message to identify timepoint - similar to a git commit message
#' @param log_data optional boolean, output a dataset log
#'
#' @importFrom dplyr mutate
#' 
#' @return data.frame with added trackr_id column
#' @export

trackr_new <- function(dataframe, trackr_dir = NULL, timepoint_message = NULL, log_data = FALSE){
  
  if (is.null(trackr_dir)){stop('No trackr_dir specified. Please specify where to store trackr log files.')}
  
  if (!is.data.frame(dataframe)) stop("dataframe must be a data.frame")
  
  hash_string <- trackr_hash(dataframe)
  
  if(!c(hash_string$hash %>% unique() %>% length()) == c(hash_string$hash %>% length())){
    stop('Duplicate rows detected. Unable to create unique record ids.')
  }
  
  file_hash <- get_file_hash(hash_string)
  
  #write reference file here - could be a function
  write_new_trackr_file(hash_string, file_hash, timepoint_message, trackr_dir)
  
  if(log_data){
    write_data_log(dataframe, trackr_dir, file_hash)
  }
  
  #trackr_id is file_hash + '_' + record_hash
  dataframe <- dataframe %>% dplyr::mutate(trackr_id = paste0(file_hash, '_', hash_string$hash))
  
  return(dataframe)
  
}

#combine_trackr function

#code_cov - public - travis - badges - and documentation "how it works"

#trackr_summary(trackr_path) 
#on docs site - website to upload and parse a trackr summary file - network graph & search function

#warn about existing files in trackr directory


#on update - drop any records that have identical old hash and new hash?

#query hash history - return a lineage csv file with step id 

#validate log file

#what about dropping duplicate hash ids

#quickly put docs together for existing functions

#prompt about deleting trackr files in a dir with new_trackr

# error when tryign to save update in a dir without a root file

#write convenience tests for development to ensure that ids remain chained together etc. 
#test when trackr_dir is not supplied

#unit tests - test for each built in error case, test for in and out columns, test for data type input and outputs
#everythign tested - simple tests
#unittests are not integration tests
#write lots of test_thats in one test-x.R file -  quick tests for each test case - not one mega test
#detailed docs
#site deployment
#record querying options


