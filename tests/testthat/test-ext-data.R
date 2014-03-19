context("ExternalData")

test_that("Testing External Data Availability", {
  expect_that(length(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread")) > 0, is_true())
})
