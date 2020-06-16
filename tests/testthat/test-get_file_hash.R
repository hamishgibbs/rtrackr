testthat::test_that('get_file_hash works as expected.', {
  source('test_data/test_df.R')
  
  output <- get_file_hash(test_df1, 123)
  
  expected_hash <- digest::sha1(paste0(123, digest::sha1('a1'), digest::sha1('b2'), digest::sha1('c3'), collapse = ''))
  
  testthat::expect_identical(output, expected_hash)
  
})