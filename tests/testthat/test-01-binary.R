context("BinaryFile")

test_that("Testing External Data Availability", {
  expect_that(length(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread")) > 0, is_true())
})

test_that("Testing BinaryFile Class", {
  
  # load
  expect_that(test <- isoread:::BinaryFile(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread")), is_a("BinaryFile"))
  expect_that(test$process(), throws_error()) # not loaded yet
  expect_that({
    test$load()
    nrow(test$keys)
  }, equals(4294)) # number of keys found in the test file
  
  # process header
  expect_that(test$move_to_key("unknown key"), throws_error()) # unknown key
  expect_that(test$move_to_key("CRawDataScanStorage", occurence = 5), throws_error()) # unknown occurence
  expect_that((move <- test$move_to_key("CRawDataScanStorage")) > 1, is_true())
  expect_that(test$skip(14), equals(move + 14))
  expect_that(test$parse("UTF16", length = 13, id = "data_trace_name"), equals("Trace Data H2"))
  test$skip(20)
  expect_that(test$parse("long", id = "n_measurements"), equals(8605))
  expect_that(test$parse("short", id = "n_ions"), equals(2))
  test$skip(29)
  expect_that(test$parse("short", id = "n_ions2"), equals(2))
  
  # read mass2/mass3 data trace
  expect_that(nrow(test$parse_array(
    types = c(time = "float", mass2 = "double", mass3 = "double"), 
    n = test$data$n_measurements, id = "trace")),
    equals(test$data$n_measurements))
  
  # footer
  test$skip(70)
  expect_that(test$parse("UTF16", length=6), equals("Mass 2"))
  test$skip(4)
  expect_that(test$parse("UTF16", length=6), equals("Mass 3"))
  
  # ratio data header
  expect_that(test$move_to_key("CRatioDataScanStorage") > 1, is_true())
  test$skip(14)
  expect_that(test$parse("UTF16", length = 13, id = "data_ratio_name"), equals("Ratio Data H2"))
  test$skip(20)
  expect_that(test$parse("long", id = "n_ratio_measurements"), equals(test$data$n_measurements))
  expect_that(test$parse("short", id = "n_ratios"), equals(1))
  test$skip(18)
  expect_that(test$parse("short", id = "n_ratios2"), equals(1))
  
  # data
  expect_that(nrow(test$parse_array(
    types = c(time = "float", ratio_3o2 = "double"), 
    n = test$data$n_ratio_measurements, id = "ratio")),
    equals(test$data$n_measurements))
  
  # other information
  expect_that(test$move_to_key("H3 Factor") > 1, is_true())
  test$skip(8)
  expect_that(test$parse("double", id = "H3") > 2, is_true()) # rough check for a reasonable H3 factor
  expect_true(length(test$find_key(".gcm$")$value) > 0, label = "Looking for gc program file name")
  expect_true(length(test$find_key(".met$")$value) > 0, label = "Looking for MS method")
  expect_true(length(test$find_key(".cf$")$value) > 0)
  expect_true(length(test$find_key("Internal")$value) > 0)
})