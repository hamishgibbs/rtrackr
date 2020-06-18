testthat::test_that('trackr_timepoint works as expected', {
  #trackr_dir is null
  testthat::expect_error(new_trackr(1))
  
  #data frame is not a dataframe
  testthat::expect_error(new_trackr(1, trackr_dir = 'a'))
  
  #trackr_id column present
  error_df <- data.frame(a = c('a'), b = c(1), trackr_id = c('a'))
  
  testthat::expect_error(new_trackr(error_df, trackr_dir = 'a'))
  
  #duplicate record present
  error_df <- data.frame(a = c('a', 'b', 'c', 'c'), b = c(1, 2, 3, 3))
  
  testthat::expect_error(new_trackr(error_df, trackr_dir = 'a'))
  
  #column named trackr_old_hash
  df <- data.frame(a = c('a', 'b', 'c'), b = c(1, 2, 3)) %>% 
    mutate(trackr_old_hash = 1)
  
  testthat::expect_error(trackr_timepoint(df, trackr_dir = 'a'))
  
})