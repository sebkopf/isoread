isoread
=======

R interface to IRMS (isotope ratio mass spectrometry) file formats typically used in stable isotope geochemistry. 

This package allows the reading and processing of stable isotope data directly from the data files and thus provides a tool for reproducible data reduction. This package is definitely still a work-in-progress, however the master branch will always be a functional version (get the 'dev' branch for the active development version) and I'll make an effort to keep it backwards compatible as it evolves. 

The underlying object structure of the package is designed to allow expansion towards a number of different types of data (the uml diagram contains a rough visual sketch of the class hierarchy) but currently, the only supported format (the only one I have test data from and had time to implement) are files containing compound specific hydrogen isotope data but expansions will come over time.

##Installation

Hadley Wickham's **devtools** package provides a super convenient way of installing ```R``` packages directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) # development tools
```

Then simply install the latest version of **isoread** directly from GitHub by running the following code (if it is the first time you install the **isoread** package, all missing dependencies will be automatically installed as well -> **ggplot2, plyr, reshape2, stringr** as well as their respective dependencies, which might take a minute, except for the **isotopia** package which is not on CRAN yet - see code below):
```coffee
library(devtools)
install_github('isotopia', 'sebkopf') # not on CRAN yet
install_github('isoread', 'sebkopf')
```

##Examples

The following example can be run with the test data provided by the **isoread** package and illustrates the direct reading of a compound-specific hydrogen isotope dataset from the binary data file. A summary of the retrieved data can be printed out via ```$show()``` and both ```$plot()``` and ```$ggplot()``` commands for the data set are already fully implemented and provide an easy quick way for visualization (of course you can access all the raw data in the object as well via ```$get_mass_data()``` and ```$get_ratio_data()``` and process it as needed). Please use the help files in R for details on functions and paramters (e.g. via ```?isoread``` - note: the object methods' help files are not supported by ```Roxygen``` yet but this is [currently being implemented](http://lists.r-forge.r-project.org/pipermail/roxygen-devel/2014-January/000456.html) so will come soon!).

```coffee
library(isoread)
obj <- isoread(system.file("extdata", "6520__F8-5_5uL_isodat2.cf", package="isoread"), type = c("H_CSIA"))
obj$show()
obj$plot()
obj$ggplot()
```
