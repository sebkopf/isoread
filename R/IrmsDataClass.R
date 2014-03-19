#' IrmsData reference class
#' @note not sure yet what's best to abstract into this one
IrmsData <- setRefClass(
  "IrmsData",
  fields = list (
    plotOptions = 'list'
    ),
  methods = list(
    #' constructor
    #' @param plotOptions list of custom plotOptions
    initialize = function(plotOptions = list(), ...) {
      callSuper(...)
      initDefaultPlotOptions(plotOptions)
    },
    
    #' Set plot options
    #' @example setSettings(a=5, b='test', ...)
    #' @example setSettings(list(a=5, b='test', ...))
    setPlotOptions = function(...) {
      # if the first argument is an unnamed list then just use this list
      if ( nargs() == 1L && is.list(..1) &&
             (is.null(names(list(...))) || (names(list(...))[[1L]] == "")) )  {
        options <- ..1
      } else 
        options <- list(...)
      
      # update plotOptions
      plotOptions <<- modifyList(plotOptions, options)
    },
    
    #' Set the defaul plot options
    #' @param ... additional plot options to set
    #' @note override in derived classes
    initDefaultPlotOptions = function(...) {
      plotOptions <<- list() # reset
      setPlotOptions(...)
    },
    
    #' Plot data
    plot = function(...) {
      
    },
    
    #' ggplot data
    ggplot = function(...) {
      
    }
    )
)