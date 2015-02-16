isoread
=======

R interface to IRMS (isotope ratio mass spectrometry) file formats typically used in stable isotope geochemistry. 

This package allows the reading and processing of stable isotope data directly from the data files and thus provides a tool for reproducible data reduction. This package is definitely still a work-in-progress, however the master branch will always be a functional version (get the 'dev' branch for the active development version) and I'll make an effort to keep it backwards compatible as it evolves. 

Currently, **isoread** supports reading files containing compound specific hydrogen isotope data, as well as clumped carbonate dual inlet data. The underlying object structure of the package is designed to allow easy expansion towards a number of different types of data and both supported file types are dynamically implemented and should be easily expandable to other continuous flow and dual inlet isotope data files, so expansions will hopefully come over time.

## How to use the isoread package

### Installation

Hadley Wickham's **devtools** package provides a super convenient way of installing ```R``` packages directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) 
```
Then simply install the latest version of **isoread** directly from GitHub by running the following code (if it is the first time you install the **isoread** package, all missing dependencies will be automatically installed as well + their respective dependencies, which might take a minute, except for the **isotopia** package which is not on CRAN yet and requires manual installation - see code below):
```coffee
library(devtools)
install_github('sebkopf/isotopia') # not on CRAN yet
install_github('sebkopf/isoread')
```

### Examples

The following examples can be run with the test data provided by the **isoread** package and illustrate the direct reading of isotope data from the binary data files. Please use the help files in R for details on functions and paramters (e.g. via ```?isoread``` - note: the object methods' help files are not supported by ```Roxygen``` yet but this is [currently being implemented](http://lists.r-forge.r-project.org/pipermail/roxygen-devel/2014-January/000456.html) so will come soon!).

#### Continuous flow

The following example llustrates the direct reading of a compound-specific hydrogen isotope dataset from the binary data file. A summary of the retrieved data can be printed out via ```$show()``` and both ```$plot()``` (fast plotting of the chromatographic data) and ```$make_ggplot()``` (ggplot that is slower but easy to manipulate). Of course, you can access all the raw data in the object as well via ```$get_mass_data()``` and ```$get_ratio_data()``` and process it as needed). 

```coffee
library(isoread)
obj <- isoread(
  system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), 
  type = c("H_CSIA"))
obj$show()
obj$plot()
obj$make_ggplot()
```

For a more detailed introduction, **check out the [continuous flow intro](inst/doc/continuous_flow_intro.Rmd)** and the resulting [HTML output](https://rawgit.com/sebkopf/isoread/master/inst/doc/continuous_flow_intro.html)!

#### Dual Inlet

Thanks to a push from [Max Lloyd](https://github.com/maxmansaxman) and example provided by Paul Gauthier, **isoread** now has basic support for dual inlet isotope data and specifically supports reading clumped CO2 runs. The following example illustrates the direct reading of a clumped CO2 dual inlet dataset from the binary data file, and prints out a summary of the retrieved data via ```$show()``` and ```$make_ggplot()```. 

```coffee
library(isoread)
obj <- isoread(
  system.file("extdata", "dual_inlet_clumped_carbonate.did", package="isoread"), 
  type = "CO2_CLUMPED")
obj$show()
obj$make_ggplot()
```

For a more detailed introduction, **check out the [dual inlet intro](inst/doc/dual_inlet_intro.Rmd)** and the resulting [HTML output](https://rawgit.com/sebkopf/isoread/master/inst/doc/dual_inlet_intro.html)!

## Development

If you have use cases for **isoread** that are not currently supported, please make use of the [Issue Tracker](https://github.com/sebkopf/isoread/issues) to collect feature ideas, expansion requests, and of course bug reports. If you are interested in helping with development, that's fantastic! Please fork the repository and branch off from the [dev branch](https://github.com/sebkopf/isoread/tree/dev) since it contains the most up-to-date development version of **isoread**. Make sure to write [```testthat``` tests](http://r-pkgs.had.co.nz/tests.html) for your work (stored in the tests/testthat directory). All tests can be run automatically and continuously during development to make it easier to spot any code problems on the go. The easiest way to run them is by running ```make autotest``` in the **isoread** directory from command line (it will test everything automatically in a completely separate R session).
