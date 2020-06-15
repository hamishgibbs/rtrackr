testthat::setup({
  dir.create('test_trackr_dir')
})

testthat::teardown({
  unlink('test_trackr_dir', recursive = F)
})

#feel free to delete this test
testthat::test_that("Single history workflow works as expected", {
  df <- data.frame(a = c('a', 'b', 'c'), b = c(1, 2, 3))
  
  trackr_dir <- 'test_trackr_dir'
  
  #setup a new trackr
  t <- trackr_new(df, trackr_dir = trackr_dir, timepoint_message = 'Start', log_data = T)
  
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "66ad329e39e0c21ef90dc3733aca530b98c06229.json")))
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "66ad329e39e0c21ef90dc3733aca530b98c06229_dl.json")))
  
  #make some change to the data 
  t <- t %>% dplyr::mutate(b = b + 1)
  
  #log this change to the data
  t <- trackr_timepoint(t, trackr_dir = trackr_dir, timepoint_message = 'Change point #1', log_data = T)
  
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "f481ea826505ad181a77f693fb74a77ceb70afab.json")))
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "f481ea826505ad181a77f693fb74a77ceb70afab_dl.json")))
  
  #repeat for subsequent processing steps
  t <- t %>% dplyr::mutate(b = b + 300)
  
  t <- trackr_timepoint(t, trackr_dir = trackr_dir, timepoint_message = 'Change point #2', log_data = T)
  
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "cbc4463c5b8d105ac7631c16d0f2a9bdc2d0183e.json")))
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "cbc4463c5b8d105ac7631c16d0f2a9bdc2d0183e_dl.json")))
  
  #splitting a single record into multiples
  t <- rbind(t, t %>% dplyr::mutate(b = b -300))
  
  t <- trackr_timepoint(t, trackr_dir = trackr_dir, timepoint_message = 'Dividing rows', log_data = T)

  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "c5ad7f3199b13fabc73031b88a94555abbe8da97.json")))
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "c5ad7f3199b13fabc73031b88a94555abbe8da97_dl.json")))
    
  #summarising data
  t <- t %>% dplyr::group_by(a)
  
  t <- trackr_summarise(t, n = dplyr::n())
  
  #output needs to remain grouped - 
  t <- trackr_timepoint(t, trackr_dir = trackr_dir, timepoint_message = 'Summarising rows', log_data = T)
  
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "5eceebfcb79ac275c6ded11704a9aaee27671a93.json")))
  testthat::expect_true(file.exists(paste0(trackr_dir, '/', "5eceebfcb79ac275c6ded11704a9aaee27671a93_dl.json")))
  
})