testthat::setup({
  dir.create('test_trackr_dir')
})

testthat::teardown({
  unlink('test_trackr_dir', recursive = F)
})

testthat::test_that('trackr_summarise works as expected', {
  source('test_data/test_df.R')
  
  test_df <- rbind(df1, df1 %>% dplyr::mutate(b = b + 1))
  
  test_df <- trackr_new(test_df, 'test_trackr_dir')
  
  test_df <- test_df %>% dplyr::group_by(a)
  
  output <- trackr_summarise(test_df, n = dplyr::n())
  
  output <- output %>% dplyr::slice(1) %>% dplyr::pull(trackr_id)
  
  testthat::expect_equal(stringr::str_split(output, ', ') %>% unlist() %>% length(), 2)
  
  clean_trackr_dir("test_trackr_dir")
  
})

testthat::test_that('trackr_summarise errors with ungrouped data', {
  source('test_data/test_df.R')
  
  testthat::expect_error(trackr_summarise(test_df1))
  
})

  