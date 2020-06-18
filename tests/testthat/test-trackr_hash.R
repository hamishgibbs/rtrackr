testthat::test_that('trackr_hash works as expected', {
  source('test_data/test_df.R')
  
  output <- trackr_hash(df1)
  
  testthat::expect_true('hash' %in% colnames(output))
  
  testthat::expect_identical(output %>% dplyr::slice(1) %>% dplyr::pull(hash), digest::sha1('a1'))
  
})