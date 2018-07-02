context("server functions")

# Get data from HydroData packag
clip = list("Colorado Springs", 10, 10)
nhd = HydroData::findNHD(clip_unit = clip)

test_that("check get_upstream routines",{
  
  nhd_prep = prep_nhd(flines = nhd$flowlines)
  
  up_stream = try(get_upstream(flines = nhd_prep))
  
  print(!inherits(up_stream,"try-error"))
  
  # see if any of the runs failed
  check = !inherits(up_stream,"try-error")
  
  # check if no error occured
  expect_true(check)
})