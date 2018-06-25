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


test_that("check subset_nomads_rda routines",{
  
  data_file = normalizePath(list.files("../../inst/flowlinefinder/flowline-app/data/current_nc", full.names = TRUE))
  
  subset = subset_nomads_rda(comids = nhd$ids, file = data_file)

  print(!inherits(subset,"try-error"))
  
  # see if any of the runs failed
  check = !inherits(subset,"try-error")

  # check if no error occured
  expect_true(check)
})