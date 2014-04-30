#FIXME remove again
.onAttach <- function(lib, pgk) {
  isoread("/Users/sk/Dropbox/VM Windows/6520__F8-5_5uL_isodat2.cf", readChromData = TRUE, type = "H_CSIA") ->> i
}

# this is not executed automatically in autotesting (since autotesting only 
# sources all code rather than loading a package)
# instead run these lines to starte the autotest
run_autotest <- function() {
  library(testthat)
  library(isotopia)
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(gridExtra)
  source("R/utilities.R", local=FALSE)
  auto_test("R", "tests/testthat")
}