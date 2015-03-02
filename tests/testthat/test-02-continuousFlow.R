context("Isoread / Continuous Flow test")


test_that("Testing general Isodat Continous Flow File (.dxf)", {
  
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "continuous_flow_example.dxf", package="isoread"), 
                                               read_mass_data = TRUE, clean_keys = FALSE, type = "CFLOW")), 
              is_a("IsodatContinuousFlowFile"))
  expect_that(nrow(test$keys), equals(1908)) # number of keys found in the test file (after IsodatFile style cleanup!)
  
  # general infp
  expect_that(names(test$data), 
              equals(c("Info_Set", "Info_Peak", "Info_Sample", "Row", "Peak Center", 
                       "Check Ref. Dilution", "H3 Stability", "H3 Factor", "Identifier 1", 
                       "Identifier 2", "Analysis", "Comment", "Preparation", "Method"
              ))) # data fields from file
  
  expect_error(test$get_mass_data(masses = "mass1"), "mass traces .* are not defined")
  
  # mass data (just check a little bit)
  expect_that(nrow(test$get_mass_data()), equals(2307))
  expect_equal(tail(test$get_mass_data())[c("time", "mass44", "mass45", "mass46")],
               structure(list(
                 time = c(481.118011474609, 481.326995849609, 481.536010742188, 481.744995117188, 481.954010009766, 482.162994384766), 
                 mass44 = c(4.79747800039387, 4.77628920031026, 4.73969454977963, 4.72236209200094, 4.67807343643034, 4.65689465070152), 
                 mass45 = c(4.44739101742098, 4.43774859982138, 4.42617818403623, 4.39339821424312, 4.4088235530122, 4.35098338445494), 
                 mass46 = c(7.72478624724449, 7.71310196812069, 7.60602098896412, 7.51065939604156, 7.43673003303302, 7.38615912363527)), 
                 .Names = c("time", "mass44", "mass45", "mass46"), row.names = 2302:2307, class = "data.frame"))
  
  # plotting test (indirect just by checking if it works) - NOTE: plotting into a temp file so it does not generate Rplot.pdf when run from cmd line
  expect_true({
    test$set_plot_options(masses = list(mass45 = list(color = "red")))
    pdf(file = (tfile <- tempfile()))
    test$plot(tlim = c(1,4), tunits = "min")
    dev.off()
    file.exists(tfile)
  })
  expect_that(test$make_ggplot(tlim = c(1,4), tunits = "min"), is_a("ggplot"))
  
  # cleanup test
  expect_equal(test$rawdata, raw())
  test$cleanup(clean_chrom_data = FALSE, clean_keys = TRUE)
  expect_equal(test$keys, data.frame())
  test$cleanup(clean_chrom_data = TRUE)
  expect_equal(test$massData, data.frame())
  expect_error(test$get_mass_data(), "No chromatographic data available")
  
  # data table test
  expect_equal(names(test$get_data_table()),
               c("Component", "Start", "Rt", "End", "Ampl 44", "Ampl 45", "Ampl 46", "Nr.", "rIntensity 44", 
                 "rIntensity 45", "rIntensity 46", "rIntensity All", "Intensity 44", 
                 "Intensity 45", "Intensity 46", "Intensity All", "Sample Dilution", 
                 "rR 45N2O/44N2O", "rR 46N2O/44N2O", "Is Ref.?", "R 45N2O/44N2O", 
                 "Ref. Name", "rd 45N2O/44N2O", "d 45N2O/44N2O", "R 46N2O/44N2O", 
                 "rd 46N2O/44N2O", "d 46N2O/44N2O", "R 18O/16O", " 18O/16O", "AT% 15N/14N", 
                 "R 15N/14N", " 15N/14N", "AT% 18O/16O", "R 17O/16O", "d 17O/16O"
               ))
  
  # refrence peak tests and changes
  expect_equal(nrow(test$get_data_table()), 8)
  expect_equal(nrow(test$get_data_table(type = "ref")), 1)
  expect_equal(nrow(test$get_data_table(type = "data")), 7)
  expect_that(test$plot_refs(`Intensity 44`), is_a("ggplot"))
  
  # peak finding, identification and mapping
  expect_equal(test$get_peak_nr_by_rt(c(30, 350)), c(1, 6))
  expect_equal(round(test$get_peak_by_rt(350)$End, 3), 352.165)
})


test_that("Testing specific Isodat Hydrogen Continous Flow (.cf) File Class (H_CSIA)", {
  
  expect_error(isoread("test", type = "C_CSIA"), "not a currently supported file type")
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), 
                                               read_mass_data = TRUE, clean_keys = FALSE, type = "H_CSIA")), 
              is_a("IsodatHydrogenContinuousFlowFile"))

  # key tests
  expect_that(nrow(test$keys), equals(3120)) # number of keys found in the test file (after IsodatFile style cleanup!)
  expect_that(names(test$data), equals(c('data_trace_name', 'n_measurements', 'n_ions',  
       'trace1_name', 'trace2_name', 'data_ratio_name', 'n_ratio_measurements', 
       'n_ratios', 'H3factor', 'GCprogram', 'MSprogram', 'Filename', 
       'ASprogram'))) # data fields from file
  
  expect_error(test$get_ratio_data(masses = "ratio6o2"))
  
  # plotting test with masses and ratios only
  expect_that(test$make_ggplot(masses = NULL), is_a("ggplot"))
  expect_that(test$make_ggplot(ratios = NULL), is_a("ggplot"))
  
  # peak table test
  expect_true(nrow(test$dataTable) > 0) # peak table loaded
  expect_true(length(setdiff(test$dataTableColumns$column, names(test$dataTable))) == 0) # all columns defined
  expect_true(all(sapply(test$dataTable, class, simplify=T) == test$dataTableColumns$type)) # all correct data type
  
  # newer versions of isodat
  expect_warning(isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.5.cf", package="isoread"), type = "H_CSIA"), "files from isodat 2.5 and 3.1 .* are currently not yet supported")
  expect_warning(isoread(system.file("extdata", "6520__F8-5_5uL_isodat3.1.cf", package="isoread"), type = "H_CSIA"), "files from isodat 2.5 and 3.1 .* are currently not yet supported")
  
  # different resaved versions of the same file
  expect_identical(
    suppressMessages(
    isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), type = "H_CSIA")$get_data_table()),
    suppressMessages(
    isoread(system.file("extdata", "6520__F8-5_5uL_isodat2_resaved.cf", package="isoread"), type = "H_CSIA")$get_data_table())
    )
  
  # refrence peak tests and changes
  expect_equal(nrow(test$get_data_table()), 19)
  expect_equal(nrow(test$get_data_table(type = "ref")), 5)
  expect_equal(nrow(test$get_data_table(type = "data")), 14)
  expect_that(test$plot_refs(), is_a("ggplot"))
  
  expect_equal({ # add extra reference peak
    test$set_ref_peaks(612, set = TRUE)
    test$get_data_table(type = "ref")$`Peak Nr.`
  }, c(3, 4, 7, 10, 13, 16))
  
  expect_equal({ # remove reference peaks
    test$set_ref_peaks(c(670, 1055), set = FALSE)
    test$get_data_table(type = "ref")$`Peak Nr.`
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
  expect_equal(test$massData, data.frame())
})

test_that("Testing isoread whole folder read", {
  # read all .cf files in extdata folder
  expect_that(isofiles <- 
                suppressMessages(suppressWarnings(
                  isoread_folder(system.file("extdata", package="isoread"), 
                                 ext=".cf", warn = FALSE, 
                                 type = c("H_CSIA")))
                ), is_a("list")) # WARN attribute will be deprecated in the future
  
  # make sure they can all be printed
  expect_true({
    pdf(file = (tfile <- tempfile()))
    sapply(isofiles, function(i) i$plot())
    dev.off()
    file.exists(tfile)
  })
})


