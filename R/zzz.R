#FIXME remove again (just a helper during development)
# automatically create a global instance of a test file on attachment of the package
.onAttach <- function(libname, pkgname) {
  i <<- suppressMessages(isoread(system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread"), type = "CLUMPED"))
  i$load()
}

# for auto-testing, just run 'make autotest' on a terminal in the isoread folder
