testthat::test_that('extract_parent_file_hash works as expected', {
  row_hash <- digest::sha1('ai')
  
  test_df <- data.frame(trackr_old_hash = paste0(digest::sha1(row_hash), '_', row_hash))
    
  output <- extract_parent_file_hash(test_df)
  
  testthat::expect_identical(output, digest::sha1(row_hash))
  
})