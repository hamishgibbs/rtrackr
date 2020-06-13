testthat::test_that('get_file_hash works as expected.', {
  source('test_data/test_df.R')
  
  hash_string <- trackr_hash(test_df1)
  
  output <- get_file_hash(hash_string)
  
  expected_hash <- digest::sha1(paste0(digest::sha1('a1'), digest::sha1('b2'), digest::sha1('c3'), collapse = ''))
  
  testthat::expect_identical(output, expected_hash)
  
})