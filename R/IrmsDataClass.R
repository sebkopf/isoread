#' IrmsData reference class
#' 
#' @name IrmsData
#' @field plotOptions holds information about default plotting options
IrmsData <- setRefClass(
  "IrmsData",
  fields = list (
    plotOptions = 'list', # stores options for plotting the data
    massData = 'data.frame', # stores raw data for all measured masses (e.g. voltages)
    dataTable = 'data.frame', # stores processed data table (=data summary)
    dataTableColumns = 'data.frame' # the columns of the data table
    ),
  methods = list(
    #' constructor
    #' @param plotOptions list of custom plotOptions
    initialize = function() {
      init_irms_data()
    },
    
    init_irms_data = function() {
      "initialize irms data container"
      
      # template for plot options
      plotOptions <<- list(
        masses = list() # example entry: mass46 = list(label = "Mass 46", color="black") 
      )
      
      # template for dataTableColumn definitions
      # data - name of the column header for in the data
      # column - name of the column stored in the data table
      # units - units of the data are in
      # type - which mode it is (character, numeric, logical, Ratio, Abundance, Delta, etc.)
      # show - whether to show this column in standard data table outputs
      dataTableColumns <<- data.frame(data = character(), column = character(), units = character(), type = character(), show = logical(), stringsAsFactors = FALSE)
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
    
    # DATA CHECKS ============================
    
    #' check internal consistency of data
    check_data = function(...) {
      check_mass_data(...)
      check_data_table(...)
    },
    
    check_mass_data = function(...) {
      "checks the consistency of the raw mass data" 
    },
    
    check_data_table = function(...) {
      "checks the consistency of the table data" 
    },
    
    # DATA RETRIEVAL ==============
    
    #' get data for masses
    #' @param masses which masses to retrieve, all defined ones by default
    #' @param melt whether to melt the data frame
    get_mass_data = function(masses = names(.self$plotOptions$masses), melt = FALSE, ...) {
      "get the mass trace data for specific masses, can be provided in \\code{melt = TRUE} long format
      for easy use in ggplot style plotting"
      stop("not implemented for this class")
    },
    
    get_data_table = function(...) {
      "retrieve the data table"
      stop("not implemented for this class")
    },
    
    # PLOTTING ===================
      
    #' plot data
    plot = function(...) {
      "plot data with standard plot functions (fast) to standard output"
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