context("dualInlet")

test_that("testing dual inlet binary files",{
  expect_that(test <- isoread:::BinaryFile(system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread")), is_a("BinaryFile"))

  expect_that({
    test$load()
    nrow(test$keys)
  }, equals(1842))
  
  expect_true(test$move_to_key("CDualInletRawData") > 1)
  
  expect_true(test$move_to_key("CDualInletEvaluatedData") > 1)
  
  stop("found: ", as.integer(test$keys[which(grepl("Background:", test$keys))[1], "byteEnd"]) + 1L)
  expect_true(test$move_to_key("Background:") > 1)
  test$pos <<- as.integer(test$keys[which(grepl("Background:", test$keys))[1], "byteEnd"]) + 1L
  #this fails? why? what's with the semicolons?
  
})


test_that("testing isoread with type CLUMPED", {
  
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread"), type = "CLUMPED")), is_a("IsodatFile"))
  
  #stop(paste0(round(test$data$standard_0, 2), collapse=", "))
  
  #expect_that(round(test$data$standard_0, 2), equals(c(15941.14, 18995.66, 21954.19, 2512.35, 29.8, -180.98)))

})