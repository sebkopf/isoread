#FIXME remove again (just a helper during development)
.onAttach <- function(libname, pkgname) {
  #isoread("/Users/sk/Dropbox/VM Windows/6520__F8-5_5uL_isodat2.cf", readChromData = TRUE, type = "H_CSIA") ->> i
}

# for auto-testing, just start R in separate console inside the package and
# run the following code:
# library(testthat)
# auto_test_package(pkg=".")