#' IrmsData reference class
#' 
#' @name IrmsData
#' @field plotOptions holds information about default plotting options
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
    
    init_irms_data = function() {
      "initialize irms data container"
      plotOptions <<- list()
    },
    
    #' @example setSettings(a=5, b='test', ...)
    #' @example setSettings(list(a=5, b='test', ...))
    set_plot_options = function(...) {
      "set plot options"
      
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
    
    plot = function(...) {
      stop("not implemented for this class")
    },
    
    #' ggplot data
    make_ggplot = function(...) {
      "generate a ggplot object for the data in this IrmsData object"
      stop("not implemented for this class")
    },
    
    summarize = function (file, ....) {
      "summarize the data stored in this object and save it to file"
      stop("not implemented for this class")
    },
    
    #' export data to csv
    export_data = function(file, ...) {
      "export the data stored in this object to file"
      stop("not implemented for this class")
    }
    )
)