#' @include IrmsDataClass.R
NULL

#' IrmsScanData reference class
#' @name IrmsScanData
#' @exportClass IrmsScanData
#' @field massData stores the scan data,inherited from IrmsDataClass
#' @seealso \link{IrmsData}
IrmsScanData <- setRefClass(
  "IrmsScanData",
  contains = "IrmsData",
  fields = list (),
  methods = list(
    
    init_irms_data = function() {
      callSuper()
      
      # default plot options
      set_plot_options(
        labels = list(x = "Steps", y = "Signal [mV]") 
      )
    },
    
    # DATA CHECKS ============================
    
    check_mass_data = function(...) {
      # check if the masses defined in plotOptions actually exist
      if (ncol(massData) == 0)
        stop("No raw data is loaded. Make sure to run load() to load all data from the file.", 
             call. = FALSE)
      
      missing <- setdiff(
        names(.self$plotOptions$masses), 
        grep("mass\\d+", names(massData), value = T))
      
      if ( length(missing) > 0 ) 
        stop("Not all masses appear to be recorded in this file, missing: ", paste(missing, collapse = ", "), call. = FALSE)
      
      return(TRUE)  
    },
    
    # DATA RETRIEVAL ==============
    
    get_mass_data = function(masses = names(.self$plotOptions$masses), melt = FALSE) {
      check_mass_data()
      
      if (length(missing <- setdiff(masses, names(massData))) > 0) 
        stop("Some masses ('", paste(missing, collapse = ", ") ,"') do not exist in the loaded massData.")
      
      if (!melt) # wide format
        return(massData[c("step", masses)])
      else # long format
        return(melt(massData[c("step", masses)], 
                    plyr::.(step), variable.name = "mass", value.name = "intensity"))
    },
    
    # PLOTTING ===================
    
    plot = function(masses = names(.self$plotOptions$masses), ...) {
      stop("not implemented yet")
    },
    
    #' ggplot data
    make_ggplot = function(masses = names(.self$plotOptions$masses), ...) {
      library(ggplot2)
      
      plot.df <- get_mass_data(masses = masses, melt = T) 
      plot.df <- merge(plot.df, data.frame(
        mass = names(plotOptions$masses), 
        mass_label = sapply(plotOptions$masses, function(x) x$label)), by="mass")
      
      p <- ggplot2::ggplot(plot.df) +
        ggplot2::aes(step, intensity, color = mass_label) + 
        ggplot2::geom_line() + 
        ggplot2::theme_bw() + 
        ggplot2::theme(legend.position = "right") + 
        ggplot2::labs(y = plotOptions$labels$y, x = plotOptions$labels$x, color = "Mass")
      
      return(p)
    }
  )
  
)