context("Export")

test_that("Testing single and multi file exports", {
  expect_that(test <- isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), readChromData = TRUE, type = "H_CSIA"), is_a("IsodatFile"))
  expect_message(test$export_data(file = tempfile(), type = "chrom"), "chrom data exported")
  expect_message(test$export_data(file = tempfile(), type = "table"), "table data exported")
  
  expect_error(export(c(new("IrmsData"), 'not an IrmsData object', new("IrmsData"))), "Not all data are IrmsData objects")
  expect_error(export(c(new("IrmsContinuousFlowData"), new("IrmsDualInletData"))), "Not all data are the same kind of IrmsData object")
  
  expect_message({
    files <- isoread(c(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), system.file("extdata", "813__F8.cf", package="isoread")),
                     type = "H_CSIA")
    export_data(files, file = tempfile())
    }, "table data exported")
  
})