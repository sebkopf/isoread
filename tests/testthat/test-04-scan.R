context("Scan")

test_that("Testing scan file",{
  # load test file
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "peakshape_example.scn", package="isoread"), type = "SCAN")), is_a("IsodatScanFile"))
  
  # @TODO add more details on the test
  # test for test$data$n to be correct
  # test for head of files to be correct
  
})