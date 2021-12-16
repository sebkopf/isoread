isoread (status: deprecated)
=======

**`isoread` is no longer maintained, please use the newer and faster implementation of this package instead: [isoreader](https://isoreader.isoverse.org/)**

If you still want to use isoread you can install it from GitHub but we do not guarantee it works in future versions of R and its dependencies.

## How to install the deprecated isoread package

Hadley Wickham's **devtools** package provides a convenient way of installing ```R``` packages directly from GitHub. To install **devtools**, run the following from the R command line:
```coffee
install.packages('devtools', depen=T) 
```
Then simply install **isoread** directly from GitHub by running the following code (if it is the first time you install the **isoread** package, all missing dependencies will be automatically installed as well + their respective dependencies, which might take a minute, except for the **isotopia** package which is not on CRAN and requires manual installation - see code below):
```coffee
library(devtools)
install_github('isoverse/isotopia', build_vignettes = TRUE) 
install_github('sebkopf/isoread', build_vignettes = TRUE)
```
