context("Isoread")

test_that("Testing Isodat Hydrogen Continous Flow File Class (H_CSIA)", {
  
  expect_error(isoread("test", type = "C_CSIA"), "not a currently supported file type")
  expect_that(test <- isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), readChromData = TRUE, clean_keys = FALSE, type = "H_CSIA"), is_a("IsodatFile"))

  # key tests
  expect_that(nrow(test$keys), equals(3127)) # number of keys found in the test file (after IsodatFile style cleanup!)
  expect_that(names(test$data), equals(c('data_trace_name', 'n_measurements', 'n_ions',  
       'trace1_name', 'trace2_name', 'data_ratio_name', 'n_ratio_measurements', 
       'n_ratios', 'H3factor', 'GCprogram', 'MSprogram', 'Filename', 
       'ASprogram'))) # data fields from file
  
  expect_error(test$get_mass_data(masses = "mass45"))
  expect_error(test$get_ratio_data(masses = "ratio6o2"))
  
  # cleanup test
  expect_equal(test$rawdata, raw())
  test$cleanup(clean_chrom_data = FALSE, clean_keys = TRUE)
  expect_equal(test$keys, data.frame())
  
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
  
  # newer versions of isodat
  expect_warning(isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.5.cf", package="isoread"), type = "H_CSIA"), "files from isodat 2.5 and 3.1 .* are currently not yet supported")
  expect_warning(isoread(system.file("extdata", "6520__F8-5_5uL_isodat3.1.cf", package="isoread"), type = "H_CSIA"), "files from isodat 2.5 and 3.1 .* are currently not yet supported")
  
  # different resaved versions of the same file
  expect_identical(
    isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), type = "H_CSIA")$get_peak_table(),
    isoread(system.file("extdata", "6520__F8-5_5uL_isodat2_resaved.cf", package="isoread"), type = "H_CSIA")$get_peak_table()
    )
  
  # refrence peak tests and changes
  expect_equal(nrow(test$get_peak_table()), 19)
  expect_equal(nrow(test$get_peak_table(type = "ref")), 5)
  expect_equal(nrow(test$get_peak_table(type = "data")), 14)
  expect_that(test$plot_refs(), is_a("ggplot"))
  
  expect_equal({ # add extra reference peak
    test$set_ref_peaks(612, set = TRUE)
    test$get_peak_table(type = "ref")$`Peak Nr.`
  }, c(3, 4, 7, 10, 13, 16))
  
  expect_equal({ # remove reference peaks
    test$set_ref_peaks(c(670, 1055), set = FALSE)
    test$get_peak_table(type = "ref")$`Peak Nr.`
  }, c(3, 7, 13, 16))
  
  # peak finding, identification and mapping
  expect_equal(test$get_peak_nr_by_rt(c(320, 860)), c(2, 7))
  expect_equal(test$get_peak_by_rt(286)$Width, 9.6)
  expect_error(test$map_peaks(data.frame(a=1:5)), "neither 'Peak Nr.' or 'Rt' defined")
  expect_warning(test$map_peaks(data.frame(Rt = 1, a=1:5)), "ignoring columns in the map not found in the peak table")
  expect_message(test$identify_peaks(c(500, 880), c("a", "b")), "no peak found at retention time")
  expect_equal({
    test$map_peaks(data.frame(Rt = c(1000, 1610), Component = c("test1", "test2"), Formula = c("C2O", "H25"), stringsAsFactors=F))
    test$get_peak_by_rt(c(1000, 1610))$Component
  }, c("test1", "test2"))
  expect_equal(test$get_peak_by_name("test1")$Component, "test1")
  expect_equal(test$get_peak_by_name(c("test1", "test2"))$Formula, c("C2O", "H25"))
  
  # data plotting
  expect_that(test$plot_data(), is_a("ggplot"))
  
  # summarizing
  expect_message(test$summarize(file = tempfile()), "Summary saved")
  
  # chrom data cleanup
  test$cleanup(clean_chrom_data = TRUE)
  expect_equal(test$chromData, data.frame())
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
