#' @include IrmsDataClass.R
NULL

#' IrmsDualInletData reference class
#' @note not implemented yet for any actual data reading
#' @name IrmsDualInletData
#' @seealso \link{IrmsData}, \link{IrmsContinuousFlowData}
IrmsDualInletData <- setRefClass(
  "IrmsDualInletData",
  contains = "IrmsData",
  fields = list (),
  methods = list(
    
    #' initialize irms data container
    init_irms_data = function(){
      callSuper()      
      
      # default plot options
      set_plot_options(
        labels = list(xmasses = "Cycle", ymasses = "Signal [mV]") # default mass data plot labels
        )
      
      # if overwriting default in derived classes, make sure to define the cycle column in dataTableColumn definitions!
      # (but it can be set to show = FALSE if desired)
      # dataTableColumns <<- data.frame(data = "cycle", column = "cycle", units = "", type = "integer", show = TRUE, stringsAsFactors = FALSE)
    },
    
    # DATA CHECKS ============================
    
    check_mass_data = function(...) {
      # check if the masses defined in plotOptions actually exist
      if (ncol(massData) == 0)
        stop("No raw data is loaded. Make sure to run load() to load all data from the file.")
      
      missing <- setdiff(
        names(.self$plotOptions$masses), 
        grep("mass\\d+", names(massData), value = T))
      
      if ( length(missing) > 0 ) 
        stop("Not all masses appear to be recorded in this file, missing: ", paste(missing, collapse = ", "))
      
      return(TRUE)  
    },
    
    check_data_table = function(...) {
      # checks the consistency of the data table and converts data types if necessary
      # by default, checks all columns defined in dataTableColumns
      
      if (ncol(dataTable) == 0)
        stop("No data table is loaded. Make sure to run load() to load all data from the file.")
      
      # check for existence of all columns
      if (length(missing <- setdiff(dataTableColumns$column, names(dataTable))) > 0) {
        # for the missing columns, try to find and convert the original data column names to the dataTable names (easier to access)
        ptc_indices <- which(dataTableColumns$column %in% missing) # indices of missing columns in dataTableColumns
        if (length(missing <- setdiff(dataTableColumns$data[ptc_indices], names(dataTable))) > 0)
          stop("Some data columns ('", paste(missing, collapse = ", ") ,"') do not exist in the loaded dataTable.")
        
        # change original column names to new name 
        pt_cols <- sapply(dataTableColumns$data[ptc_indices], function(i) which(names(dataTable) == i), simplify = TRUE)
        names(dataTable)[pt_cols] <<- dataTableColumns$column[ptc_indices]
      }
      
      # bring data table columns into right order
      dataTable <<- dataTable[dataTableColumns$column]
      
      # check for proper class and convert if necessary
      if (any(types <- (sapply(dataTable, class, simplify=T) != dataTableColumns$type))) {
        ptc_indices <- which(types) # indices of the columns to convert
        
        for (i in ptc_indices) {
          value <- convert_data(
            value = dataTable[[dataTableColumns$column[i]]], 
            data_type = dataTableColumns$type[i])
          dataTable[[dataTableColumns$column[i]]] <<- value
        }
      }
    },
    
    convert_data = function(value, data_type) {
      "function converts data table entries to their appropriate data types - overwrite in derived classes for more specialized behaviour"
      suppressWarnings(
        try(switch(
          data_type,
          "integer" = as.integer(value),
          "character" = as.character(value),
          "numeric" = as.numeric(value),
          "logical" =  as.logical(value),
          stop("data type not supported: ", data_type)),
          TRUE))
    },
    
    # DATA RETRIEVAL ===========
    
    get_mass_data = function(masses = names(.self$plotOptions$masses), melt = FALSE) {
      check_mass_data()
      
      if (length(missing <- setdiff(masses, names(massData))) > 0) 
        stop("Some masses ('", paste(missing, collapse = ", ") ,"') do not exist in the loaded massData.")
      
      if (!melt) # wide format
        return(massData[c("analysis", "cycle", masses)])
      else # long format
        return(melt(massData[c("analysis", "cycle", masses)], 
                    .(analysis, cycle), variable.name = "mass", value.name = "intensity"))
    },
    
    #' by default, returns all data table columns that are enabled with show = TRUE
    #' @param summarize whether to show whole data table or just the summary
    get_data_table = function(select = default_select(), summarize = FALSE) {
      
      default_select <- function() {
        dataTableColumns$column[dataTableColumns$show]
      }
      
      check_data_table()
      
      if (length(missing <- setdiff(select, names(dataTable))) > 0) 
        stop("Some data ('", paste(missing, collapse = ", ") ,"') do not exist in the loaded dataTable.")
      
      if (!summarize)
        return(dataTable[select])
      
      # summarize data table
      select <- select[select != "cycle"] # exclude cycle form the summary (since it gets summarized)
      summary <- ddply(melt(dataTable[select], id.vars = NULL, variable.name = "Variable"), .(Variable), 
                       plyr:::summarize,
                       Mean = mean(value),
                       `Std. Devi.` = sd(value),
                       `Std. Error.` = `Std. Devi.`/sqrt(length(value)))
      return(summary)
    },
    
    # PLOTTING ===================
    
    plot = function(masses = names(.self$plotOptions$masses), ...) {
      stop("not implemented yet")
    },
    
    #' ggplot data
    make_ggplot = function(masses = names(.self$plotOptions$masses), ...) {
      library(ggplot2)
      
      plot.df <- get_mass_data(masses = masses, melt = T) # pass masses FIXME
      plot.df <- merge(plot.df, data.frame(
        mass = names(plotOptions$masses), 
        mass_label = sapply(plotOptions$masses, function(x) x$label)), by="mass")      
      x_breaks <- seq(min(plot.df$cycle), max(plot.df$cycle), by=1)
      
      p <- ggplot(plot.df,
             aes(cycle, intensity, shape = analysis, linetype = analysis, fill = mass)) + 
        geom_line(colour = "black") + 
        geom_point(colour = "black") + 
        scale_x_continuous(breaks = x_breaks) +
        scale_shape_manual("Type", values = c(21, 22)) + 
        scale_linetype_manual("Type", values = c(1, 2)) + 
        scale_fill_manual("Mass", breaks = names(plotOptions$masses),
                          labels = vapply(plotOptions$masses, function(x) x$label, FUN.VALUE=character(1)),
                          values = vapply(plotOptions$masses, function(x) x$color, FUN.VALUE=character(1)), 
                          guide = "none") +
        theme_bw() + theme(legend.position = "bottom") + 
        facet_wrap(~mass_label, scales = "free") + 
        labs(y = plotOptions$labels$ymasses)
      
      return(p)
    }
    
  )
)