context("server functions")


test_that("check get_nomads_filelist routings",{
  
  fileList = try(get_nomads_filelist(num  = 40))
  
  # Make sure 3 vals
  expect_equal(length(fileList), 3)
  
  # Right number of urls are returned
  expect_equal(length(fileList$urls), 40)
  
})