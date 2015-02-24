#FIXME remove again (just a helper during development)
# automatically create a global instance of a test file on attachment of the package
.onAttach <- function(libname, pkgname) {
  #i <<- suppressMessages(isoread(system.file("extdata", "continuous_flow_example.dxf", package="isoread"), 
  #                               read_mass_data = TRUE, clean_keys = FALSE, type = "CFLOW"))
}

# for auto-testing, just run 'make autotest' on a terminal in the isoread folder
