testthat::test_that('remove_parent_file_hash works as expected.', {
  
  test_df <- data.frame(trackr_old_hash = 'parentfilehash_rowhash')
  
  output <- remove_parent_file_hash(test_df, 'parentfilehash')
  
  testthat::expect_identical(output$trackr_old_hash, 'rowhash')
  
})