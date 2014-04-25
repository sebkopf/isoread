context("Isoread")

test_that("Testing Isodat Hydrogen Continous Flow File Class (H_CSIA)", {
  expect_that(test <- isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), readChromData = TRUE, type = c("H_CSIA")), is_a("IsodatFile"))

  # key tests
  expect_that(nrow(test$keys), equals(3127)) # number of keys found in the test file (after IsodatFile style cleanup!)
  expect_that(names(test$data), equals(c('data_trace_name', 'n_measurements', 'n_ions',  
       'trace1_name', 'trace2_name', 'data_ratio_name', 'n_ratio_measurements', 
       'n_ratios', 'H3factor', 'GCprogram', 'MSprogram', 'Filename', 
       'ASprogram'))) # data fields from file
  
  expect_error(test$get_mass_data(masses = "mass45"))
  expect_error(test$get_ratio_data(masses = "ratio6o2"))
  
  # plotting test (indirect just by checking if it works)
  expect_true({
    test$set_plot_options(masses = list(mass3 = list(color = "red")))
    test$plot(tlim = c(10, 15), tunits = "min")
    TRUE
  })
  expect_that(test$ggplot(masses = NULL), is_a("ggplot"))
  expect_that(test$ggplot(ratios = NULL), is_a("ggplot"))
  expect_that(test$ggplot(tlim = c(10, 15), tunits = "min"), is_a("ggplot"))
  
  # peak table test
  expect_true(nrow(test$peakTable) > 0) # peak table loaded
  expect_true(length(setdiff(test$peakTableColumns$column, names(test$peakTable))) == 0) # all columns defined
  expect_true(all(sapply(test$peakTable, class, simplify=T) == test$peakTableColumns$type)) # all correct data type
})

test_that("Testing isoread whole folder read", {
  # read all .cf files in extdata folder
  expect_that(isofiles <- isoread_folder(system.file("extdata", package="isoread"), ext=".cf", warn = FALSE, type = c("H_CSIA")), is_a("list"))
  
  # make sure they can all be printed
  expect_true({
    sapply(isofiles, function(i) i$plot())
    TRUE
  })
})
