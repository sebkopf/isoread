context("Dual Inlet")

test_that("Testing general dual inlet binary file",{
  # load test file
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread"), type = "DI")), is_a("IsodatDualInletFile"))
  
  # check key number
  expect_that({test$load(); nrow(test$keys)}, equals(1842))
  
  # check presence of certain keys
  test_keys <- c("CTraceInfo", "CPlotRange", "CDualInletRawData", "DualInlet RawData Standard Block",
                 "DualInlet RawData Sample Block", "CTwoDoublesArrayData", "CDualInletEvaluatedData",
                 "CMeasurmentInfos", "CMeasurmentErrors", "Sequence Line Information", "Visualisation Informations")
  sapply(test_keys, function(key) expect_true(test$move_to_key(key) > 1))
  
  # check that masses are detected properly and data is returned properly
  test$set_plot_options(masses = list(mass42 = list(label = "Mass 42")))
  expect_error(test$check_mass_data(), "Not all masses appear to be recorded in this file")
  test$set_plot_options(masses = list(mass42 = NULL))
  expect_that({test$process(); test$get_mass_data()}, equals(structure(list(
    analysis = c("Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Standard", "Sample", "Sample", "Sample", "Sample", "Sample", "Sample", "Sample", "Standard"), 
    cycle = c(1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 0), 
    mass44 = c(15941.1363271265, 15933.5415417438, 15916.5753680181, 15901.6242158319, 15896.0021519182, 15884.6349693404, 15875.8903911327, 15955.1761050668, 15945.6432073637, 15925.9044562907, 15911.0955861597, 15909.8304295061, 15912.5607067829, 15914.033518328, 15946.4199904763),
    mass45 = c(18995.6641449228, 18986.6426935265, 18966.3386596933, 18948.599548316, 18941.9738190935, 18928.5070148353, 18918.1242702615, 19122.7104838963, 19111.1587191928, 19087.5745071665, 19069.7008046558, 19068.2356840326, 19071.5658532181, 19073.465510929, 19001.9437826595),
    mass46 = c(21954.1903700836, 21943.5768978526, 21919.9657675996, 21899.1733602254, 21891.4939747018, 21876.1851946469, 21863.9566288155, 22237.9853884335, 22224.6454843867, 22197.0543020471, 22176.1811399929, 22174.6160373849, 22178.4807256429, 22180.5188199481, 21960.6199807974),
    mass47 = c(2512.34539994474, 2511.28020726069, 2508.49706375876, 2506.10421681761, 2505.60046143274, 2503.55467620561, 2502.44431399069, 2559.24464173059, 2557.85735348105, 2554.38782454663, 2552.02286606039, 2551.66523819173, 2552.28051194425, 2552.79964917294, 2513.27984695638), 
    mass48 = c(29.8004766868714, 29.8181826767711, 29.7807423862016, 29.7630362554129, 29.7581767374068, 29.7292305805958, 29.695128630388, 31.0943601057772, 31.0915312685677, 31.0684586128672, 31.0194843214432, 31.0281917024862, 31.0420487673658, 31.0584470903803, 29.7874191651753), 
    mass49 = c(-180.975239499152, -180.774840579306, -180.458274708744, -180.231083464175, -180.309544685111, -180.201251727574, -180.034033824346, -181.245969751486, -180.997565144103, -180.700227016011, -180.465344900286, -180.273383292619, -180.511179915295, -180.373364705306, -181.27120940821)), 
    .Names = c("analysis", "cycle", "mass44", "mass45", "mass46", "mass47", "mass48", "mass49"), row.names = c(NA, 15L), class = "data.frame")))
  
  expect_error(test$get_mass_data(masses = c("mass2")), "Some masses .* do not exist in the loaded massData")
  
  # evaluated data
  expect_equal(test$get_data_table(summarize = FALSE), structure(list(
    cycle = c(1, 2, 3, 4, 5, 6, 7), 
    `d 45CO2/44CO2 ` = c(3.32877703714618, 3.32095526378628, 3.32633836566676, 3.32003038117312, 3.31874078883043, 3.31775484493191, 3.32175175548288), 
    `d 46CO2/44CO2 ` = c(37.3306472298882, 37.3117987381053, 37.3175143140962, 37.3172067628513, 37.3297678783726, 37.3228487105193, 37.3183680061899), 
    `d 13C/12C ` = c(2.19299170711751, 2.18529477038931, 2.19085700819077, 2.18410570112626, 2.18226436164115, 2.18146010548259, 2.18590870180324), 
    `d 18O/16O ` = c(37.3664562584672, 37.3476045904049, 37.353313758336, 37.3530212841975, 37.3656004334371, 37.358675484273, 37.3541796890982), 
    `d 17O/16O ` = c(6.20232096749262, 6.19288569364174, 6.19574314518756, 6.19559676140047, 6.20189262822857, 6.1984266942936, 6.19617654484061), 
    `AT% 13C/12C ` = c(1.10805634956821, 1.10804793388553, 1.10805401552957, 1.10804663377992, 1.10804462049446, 1.10804374113609, 1.10804860514639), 
    `AT% 18O/16O ` = c(0.207580927072588, 0.207577162613362, 0.207578302667939, 0.207578244264249, 0.207580756174273, 0.207579373342358, 0.20757847558426)), 
    .Names = c("cycle", "d 45CO2/44CO2", "d 46CO2/44CO2", "d 13C/12C", "d 18O/16O", "d 17O/16O", "AT% 13C/12C", "AT% 18O/16O"), row.names = c(NA, -7L), class = "data.frame"))
  
  expect_error(test$get_data_table(select = c("non column")), "Some data .* do not exist in the loaded dataTable")
  
  expect_equal(test$get_data_table(select = c("d 13C/12C", "d 18O/16O"), summarize = TRUE), 
              structure(list(Variable = structure(1:2, .Label = c("d 13C/12C", "d 18O/16O"), class = "factor"), 
                             Mean = c(2.18612605082155, 37.3569787854591), 
                             `Std. Devi.` = c(0.00430160752664701, 0.00697422407563108), 
                             `Std. Error.` = c(0.00162585482190166, 0.00263600892739416)), 
                        .Names = c("Variable", "Mean", "Std. Devi.", "Std. Error."), 
                        row.names = c(NA, -2L), class = "data.frame"))
  
  # other information
  expect_equal(test$data$`Identifier 1`, "CIT Carrara")
  expect_equal(test$data$Analysis, "49077")
  expect_equal(test$data$Info_Background, "Background: 8.87 mV,11.31 mV,12.98 mV,6.40 mV,1.90 mV,5.88 mV (old253)")
  
  # data retrieval
  expect_equal(names(test$get_mass_data(masses = c("mass46", "mass49"))), c("analysis", "cycle", "mass46", "mass49"))
  expect_equal(names(test$get_data_table(select = c("cycle", "d 45CO2/44CO2", "AT% 18O/16O"), summarize = FALSE)), 
               c("cycle", "d 45CO2/44CO2", "AT% 18O/16O"))
  
  # plotting
  expect_error(test$plot(), "not implemented yet")
  expect_that(test$make_ggplot(masses = c("mass44", "mass45")), is_a("ggplot"))
})


test_that("Testing specialized instance of the general dual inlet example", {
  
  expect_that(test <- suppressMessages(
    isoread(system.file("extdata", "dual_inlet_example.did", package="isoread"), type = "DI")), is_a("IsodatFile"))
  
  expect_equal(
    as.character(test$get_data_table(summarize = TRUE)$Variable), 
    c("d 32O2/28N2", "d 32O2/29N2", "d 40Ar/28N2", 
      "d 32O2/40Ar", "d 44CO2/28N2", "d 44CO2/40Ar"))
  
})

test_that("Testing specialized instance of dual inlet file: isodat clumped CO2", {
  
  expect_that(test <- suppressMessages(isoread(system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread"), type = "CO2_CLUMPED")), is_a("IsodatFile"))
  
  expect_equal(
    as.character(test$get_data_table(select = c("d13C", "d18O", "d17O", "at% 13C", "at% 18O"), summarize = TRUE)$Variable), 
    c("d13C", "d18O", "d17O", "at% 13C", "at% 18O"))
  
})



