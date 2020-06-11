testthat::test_that('new_trackr works as expected', {
  testthat::expect_error(new_trackr(1))
  testthat::expect_error(new_trackr('a'))
  
  dup_df <- data.frame(a = c('a', 'b', 'c', 'c'), b = c(1, 2, 3, 3))
  testthat::expect_error(new_trackr(dup_df))
  
})

testthat::test_that('trackr_timepoint works as expected', {
  df <- data.frame(a = c('a', 'b', 'c'), b = c(1, 2, 3)) %>% 
    mutate(old_hash = 1)
  
  testthat::expect_error(trackr_timepoint(df))
  
})

