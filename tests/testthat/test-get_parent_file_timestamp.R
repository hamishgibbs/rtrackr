testthat::setup({
  dir.create('test_trackr_dir')
})

testthat::teardown({
  unlink('test_trackr_dir', recursive = F)
})

testthat::test_that('get_parent_file_timestamp works as expected', {
  source('test_data/test_df.R')
  
  df1 <- trackr_new(df1, 'test_trackr_dir')
  
  parent_file_hash <- extract_parent_file_hash(df1 %>% dplyr::rename(trackr_old_hash = trackr_id))
  
  parent_fn <- paste0('test_trackr_dir/', parent_file_hash, '.json')
  
  tstamp <- get_parent_file_timestamp(parent_fn)
  
  testthat::expect_type(tstamp, "integer")
  
})