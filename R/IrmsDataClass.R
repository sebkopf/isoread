#' IrmsData reference class
#' 
#' @note not sure yet what's best to abstract into this one
IrmsData <- setRefClass(
  "IrmsData",
  fields = list (
    plotOptions = 'list'
    ),
  methods = list(
    #' constructor
    #' @param plotOptions list of custom plotOptions
    initialize = function() {
      init_irms_data()
    },
    
    #' initialize irms data class
    #' @param plotOptions list of plot options to initialize the data container with
    init_irms_data = function() {
      plotOptions <<- list()
    },
    
    #' Set plot options
    #' @example setSettings(a=5, b='test', ...)
    #' @example setSettings(list(a=5, b='test', ...))
    #' @note fix the naming schizophrenia (some are lower some opper gase)
    set_plot_options = function(...) {
      # if the first argument is an unnamed list then just use this list
      if ( nargs() == 1L && is.list(..1) &&
             (is.null(names(list(...))) || (names(list(...))[[1L]] == "")) )  {
        options <- ..1
      } else 
        options <- list(...)
      
      # update plotOptions
      plotOptions <<- modifyList(plotOptions, options)
    },
    
    #' check internal consistency of data
    check_data = function(...) {
      
    },
    
    #' Plot data
    plot = function(...) {
      
    },
    
    #' ggplot data
    ggplot = function(...) {
      
    }
    )
)