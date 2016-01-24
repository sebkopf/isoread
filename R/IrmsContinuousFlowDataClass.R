#' @include IrmsDataClass.R
NULL

#' IrmsContinuousFlowData reference class
#' @name IrmsContinuousFlowData
#' @exportClass IrmsContinuousFlowData
#' @field massData stores the chromatographic data (the actual mass and ratio data traces), 
#' inherited from IrmsDataClass
#' @field dataTable stores the peak table (detected peaks and all their information),
#' inherited from IrmsDataClass
#' @field dataTableKeys stores information about which columns correspond
#' to key elements of the dataTable (e.g. the peak number, retention time
#' and compound name)
#' @seealso \link{IrmsData}, \link{IrmsDualInletData}
IrmsContinousFlowData <- setRefClass(
  "IrmsContinuousFlowData",
  contains = "IrmsData",
  fields = list (
    dataTableKeys = 'character' # peak table column keys of importance
    ),
  methods = list(
    
    init_irms_data = function() {
      callSuper()
      
      # default plot options
      set_plot_options(
          tunits = list(value = 1, labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60)),
          labels = list(x="Time", ymasses = "Signal [mV]", yratios = "Ratio"),
          masses = list(), # example entry: mass1 = list(label = "Mass 1", color="black", offset=200) # offset in mV
          ratios = list (), # example entry: ratio1 = list(label = "Ratio 1", color="red")), #offset in dimensionells ratio units
          baseMarker = list(on = TRUE, color="red"),
          apexMarker = list(on = TRUE, color="red"),
          edgeMarker = list(on = TRUE, color="blue"))
      
      # template for dataTableColumn definitions
      # data - name of the column header for in the data
      # column - name of the column stored in the peak table
      # units - units of the data are in
      # type - which mode it is (character, numeric, logical, Ratio, Abundance, Delta)
      # show - whether to show this column in standard peak table outputs
      dataTableColumns <<- data.frame(data = character(), column = character(), units = character(), type = character(), show = logical(), stringsAsFactors = FALSE)
      
      # peak table keys (with default values)
      # peak_nr = column that identifies the peak number
      # ref_peak = column that identifies reference peak column (T/F)
      # rt = column that holds the retention time
      # rt_start = column that holds the retention time at the start of the peak
      # rt_end = column that holds the retentio time at the end of the peak
      # name = column that holds the compound names
      dataTableKeys <<- c(peak_nr = "Nr.", ref_peak = "Is Ref.?", rt = "Rt", rt_start = "Start", rt_end = "End", name = "Component")
    },
    
    # DATA CHECKS ============================
    
    check_mass_data = function(masses = names(.self$plotOptions$masses), 
                                ratios = names(.self$plotOptions$ratios), ..., warn = TRUE) {
      "checks the consistency of the chromatographic data, by default checks for 
      all masses and ratios" 
      
      if (length(missing <- setdiff(masses, names(.self$plotOptions$masses))) > 0)
        stop("Some mass traces ('", paste(missing, collapse = ", ") ,"') are not defined in plotOptions$masses.")
      
      if (ncol(massData) > 0 && length(missing <- setdiff(masses, names(massData))) > 0)
        stop("Some mass traces ('", paste(missing, collapse = ", ") ,"') are not available from the chromatographic data.")
      
      if (length(missing <- setdiff(ratios, names(.self$plotOptions$ratios))) > 0)
        stop("Some ratio traces ('", paste(missing, collapse = ", ") ,"') are not defined in plotOptions$ratios.")
      
      if (ncol(massData) > 0 && length(missing <- setdiff(ratios, names(massData))) > 0)
        stop("Some ratio traces ('", paste(missing, collapse = ", ") ,"') are not available from the chromatographic data.")
      
      if (ncol(massData) == 0 && warn)
        warning("No chromatographic data currently loaded.")
    },
    
    check_data_table = function(..., warn = TRUE) {
      "checks the consistency of the peak table and converts data types if necessary" 
      
      if (ncol(dataTable) > 0) {
        
        # check for all dataTable columns existence
        if (length(missing <- setdiff(dataTableColumns$column, names(dataTable))) > 0) {
          # for the missing columns, try to find and convert the original data column names to the dataTable names (easier to access)
          ptc_indices <- which(dataTableColumns$column %in% missing) # indices of missing columns in dataTableColumns
          if (length(missing <- setdiff(dataTableColumns$data[ptc_indices], names(dataTable))) > 0)
            stop("Some data columns ('", paste(missing, collapse = ", ") ,"') do not exist in the loaded dataTable.")
          
          # change original column names to new name 
          pt_cols <- sapply(dataTableColumns$data[ptc_indices], function(i) which(names(dataTable) == i), simplify = TRUE)
          names(dataTable)[pt_cols] <<- dataTableColumns$column[ptc_indices]
        }
        
        # bring peak columns into right order
        dataTable <<- dataTable[dataTableColumns$column]
        
        # check for proper class and convert if necessary
        if (any(types <- (sapply(dataTable, class, simplify=T) != dataTableColumns$type))) {
          ptc_indices <- which(types) # indices of the columns to convert
          if (warn) {
            #FIXME: do we need this message? got a bit too annoying
            #info <- paste0(dataTableColumns$column[ptc_indices], " (to ", dataTableColumns$type[ptc_indices], ")")
            #message("Converting data types of peak table columns: ", paste0(info, collapse = ", "))
          }
            
          for (i in ptc_indices) {
            data <- dataTable[[dataTableColumns$column[i]]]
            data <- suppressWarnings(try(switch(dataTableColumns$type[i],
                         "integer" = as.integer(data),
                         "character" = as.character(data),
                         "numeric" = as.numeric(data),
                         "logical" =  as.logical(data),
                         "Ratio" = ratio(as.numeric(data)), 
                         "Abundance" = abundance(as.numeric(data)),
                         "Delta" = delta(`2H` = as.numeric(data), major = "1H", ref = "VSMOW"), # FIXME this only really applies to hydrogencontinuousflowfileclass!
                         stop("data type not supported: ", dataTableColumns$type[i])),
                  TRUE))
            dataTable[[dataTableColumns$column[i]]] <<- data
          }
        }
        
        # bring peak rows into right order (sorted by retention time)
        dataTable[order(dataTable[[dataTableKeys["rt"]]]),] ->> dataTable
                
      } else if (warn)
        warning("No peak table data currently loaded.")
    },
    
    # DATA RETRIEVAL ==============
    
    get_data_table = function(type = c("ref", "data", "both")) {
      "retrieve the peak table"
      
      if (missing(type)) type <- "both"
      type <- match.arg(type)
      
      if (ncol(dataTable) == 0) {
        warning("No peak table data currently loaded.")
        return (NULL)
      }
      
      if (type == "ref") {
        return (dataTable[dataTable[[dataTableKeys["ref_peak"]]] == TRUE, ])
      } else if (type == "data") {
        return (dataTable[dataTable[[dataTableKeys["ref_peak"]]] == FALSE, ])
      } else {
        return (dataTable)
      }
    },
    
    #' get data for masses
    #' @param masses which masses to retrieve, all defined ones by defaul
    #' @param melt whether to melt the data frame
    #' @note consider storing the extra x units in the massData but worries about the size of this
    #' object keep me from storing extra information in massData
    get_mass_data = function(masses = names(.self$plotOptions$masses), melt = FALSE) {
      "get the mass trace data for specific masses, can be provided in \\code{melt = TRUE} format
      for easy use in ggplot style plotting"
      
      # checks
      if (nrow(massData) == 0)
        stop("No chromatographic data available.")
      check_mass_data(masses = masses, ratios = c())

      # data
      data <- subset(massData, select = c("time", masses))
      # introduce multiple x units
      for (i in seq_along(plotOptions$tunits$labels)) {
        data[[paste0("time.", plotOptions$tunits$labels[i])]] <- 
          do.call(plotOptions$tunits$funcs[[i]] , list(data$time))
      }
      # introduce trace offsets
      for (mass in masses) 
        data[[paste0(mass, ".offset")]] <- data[[mass]] + plotOptions$masses[[mass]]$offset

      # wide vs. long format
      if (melt) {
        offsets <- paste0(masses, ".offset")
        mass.melt <- melt(
          subset(data, select = which(!names(data)%in%offsets)),
          measure.vars = masses, value.name = "signal")
        offset.melt <- melt(subset(data, select = offsets), measure.vars = offsets, value.name = "signal.offset")
        return (cbind(mass.melt, offset.melt["signal.offset"]))
      } else
        return(data)
    },
    
    #' get data for ratios
    #' @param ratios which ratios to retrieve, all defined ones by default
    #' @param melt whether to melt the data frame
    get_ratio_data = function(ratios = names(.self$plotOptions$ratios), melt = FALSE) {
      "get the ratio trace data for specific ratios, can be provided in \\code{melt = TRUE} format
      for easy use in ggplot style plotting"
      
      # checks
      if (nrow(massData) == 0)
        stop("No chromatographic data available.")
      check_mass_data(masses = c(), ratios = ratios)
      
      # data
      data <- subset(massData, select = c("time", ratios))
      # introduce multiple x units
      for (i in seq_along(plotOptions$tunits$labels)) {
        data[[paste0("time.", plotOptions$tunits$labels[i])]] <- 
          do.call(plotOptions$tunits$funcs[[i]] , list(data$time))
      }
      
      # long vs. wide format
      if (melt) 
        return (melt(data, measure = ratios, value.name = "signal"))
      else
        return(data)
    },
    
    # PEAK IDENTIIFICATON AND UPDATE =============
  
    get_peak_nr_by_rt = function(rts) {
      "find peak numbers (i.e. ids) by retention time(s), returns a vector of found peak numbers (integer(0) if none found)"
      
      if (is.null(get_data_table())) 
        stop("can't search for peaks without a peak table")
      
      unlist(sapply(rts, function(rt) {
        dataTable[dataTable[[dataTableKeys["rt_start"]]] <= rt & 
                    rt <= dataTable[[dataTableKeys["rt_end"]]], dataTableKeys["peak_nr"], drop = T]
      }))
    },
    
    get_peak_nr_by_name = function(names) {
      "find peak numbers (i.e. ids) by name(s), returns a vector of found peak numbers (integer(0) if none found)"
      
      if (is.null(get_data_table())) 
        stop("can't search for peaks without a peak table")
      
      unlist(sapply(names, function(name) {
        dataTable[dataTable[[dataTableKeys["name"]]] == name, dataTableKeys["peak_nr"], drop = T]
      }))
    },
    
    #' @return returns the data frame of found peaks (0-row df if none found)
    get_peak = function(peak_nr, select = names(dataTable)) {
      "retrieve information for a peak in the peak table (identified by peak_nr), can specify which columns to retrieve
      with \\code{selec}, retrieves all columns by default"
      
      if (is.null(get_data_table())) 
        stop("can't search for peaks without a peak table")
      dataTable[dataTable[[dataTableKeys["peak_nr"]]] %in% peak_nr, select]
    },
    
    #' Get peaks by rt
    #' @param rts 
    get_peak_by_rt = function(rts, select = names(dataTable)) {
      "retrieve information for peak(s) in the peak table (identified by retention times)"
      peak_nrs <- get_peak_nr_by_rt(rts)
      get_peak(peak_nrs, select = select)
    },
    
    #' Get peaks by name
    #' @param rts 
    get_peak_by_name = function(names, select = names(dataTable)) {
      "retrieve information for peak(s) in the peak table (identified by names)"
      peak_nrs <- get_peak_nr_by_name(names)
      get_peak(peak_nrs, select = select)
    },
    
    #' Set peak(s) columns (this one should not be used externally)
    #' @param peak_nr - peak number(s) to update with the provided attributes
    #' @param ... - peak columns to change (only one value per attribute allowed!)
    set_peak = function(peak_nr, ...){
      
      if (is.null(get_data_table())) 
        stop("can't change peaks without a peak table")
      
      attribs <- list(...)
      if (length(attribs) == 1L && is(attribs[[1]], "list"))
        attribs <- attribs[[1]]
      
      if (any(! names(attribs) %in% names(dataTable)))
        stop("not all attributes names are defined in the peak table")
      if (any(sapply(attribs, length) != 1))
        stop("multiple values supplied, only exactly one per attribute allowed")
      dataTable[dataTable[[dataTableKeys["peak_nr"]]] %in% peak_nr, names(attribs)] <<- attribs
    },
    
    #' set peak(s) columns by retention time
    set_peak_by_rt = function(rts, ...) {
      peak_nrs <- get_peak_nr_by_rt(rts)
      set_peak(peak_nrs, ...)
    },
    
    #' Set/unset reference peaks
    #' 
    #' Identify peaks as reference peaks (or remove their status as a reference peak).
    #' This is a specialized call for \code{set_peak_by_rt}
    #' 
    #' @param rt (can be a vector)
    #' @param set - wether to set it to be or not be a reference peak
    #' @param reevalute - whether to revaluate the peak table right away
    set_ref_peaks = function(rts, set = TRUE, reevaluate = FALSE) {
      " Identify peaks (by their retention times) as reference peaks (or remove their status as a reference peak)"
      
      set_peak_by_rt(rts, setNames(list(set), dataTableKeys["ref_peak"]))
      if (reevaluate)
        reevaluate_peak_table()
    },
    
    #' Identify peak(s)
    #' 
    #' Identify peaks by mapping compound names to retention times
    #' 
    #' @param rts - retention times
    #' @param compounds - compound names
    identify_peaks = function(rts, compounds) {
      "Identify peaks by mapping compound names to retention times"
      if (length(rts) != length(compounds))
        stop("not the same number of compounds and retention times supplied")
      map <- data.frame(rts, compounds, stringsAsFactors = F)
      names(map) <- c(dataTableKeys["rt"], dataTableKeys["name"])
      map_peaks(map)
    },
    
    #' Map peak(s)
    #' 
    #' Add information to peaks by mapping properties from a data frame
    #' that contains at least the defined \code{dataTableKeys['peak_nr']} or 
    #' \code{dataTableKeys['rt']} as a column. Additional columns (other than peak
    #' nr and retention time) are mapped
    #' to the relevant peaks if they correspond to existing columns, otherwise
    #' they are disregarded with a warning.
    #' 
    #' @note make sure to have the data.frame that is passed in with
    #' stringsAsFactors = F
    #' 
    #' @param map - the map of properties
    map_peaks = function(map) {
      "Add information to peaks by mapping properties from a data frame
      that contains at least the defined peak number (e.g. `Peak Nr.`) or 
      retention time (Rt) as a column. Additional columns (other than peak
      nr and retention time) are mapped to the relevant peaks if they correspond 
      to existing columns, otherwise they are disregarded with a warning. 
      
      Note: make sure to have the data.frame that is passed in set with
      \\code{stringsAsFactors = F} (usually the desired setting for the mapping)"
      
      if (is.null(get_data_table())) 
        stop("can't map peaks without a peak table")
      
      if (dataTableKeys['peak_nr'] %in% names(map)) {
        # found peak nrs
        nrs <- as.list(map[[dataTableKeys['peak_nr']]])
      } else if (dataTableKeys['rt'] %in% names(map)) {
        nrs <- sapply(map[[dataTableKeys['rt']]], function(rt) list(get_peak_nr_by_rt(rt))) 
      } else {
        stop ("neither '", dataTableKeys['peak_nr'], "' or '", dataTableKeys['rt'], "' defined in the map. ",
              "not clear what to identify peaks by")
      }
      
      columns <- names(map)[!names(map) %in% c(dataTableKeys['peak_nr'], dataTableKeys['rt'])]
      existing <- intersect(columns, names(dataTable))
      ignored <- setdiff(columns, names(dataTable))
      if (length(ignored > 0)) 
        warning("ignoring columns in the map not found in the peak table: ", paste(ignored, collapse = ", "))
      
      # go mapping
      if (length(existing) > 0) {
        for (i in 1:nrow(map)) {
          if (length(nrs[[i]]) == 0) 
            message("no peak found at retention time ", map[i, dataTableKeys['rt']], 
                    " (map entry: ", paste(map[i, ], collapse = ", "), ")") # used to be warning!
          else if (length(nrs[[i]]) > 1) 
            message("more than one peak found at retention time ", map[i, dataTableKeys['rt']], 
                    " (map entry: ", paste(map[i, ], collapse = ", "), ")") # used to be warning!
          set_peak(nrs[[i]], as.list(map[i, existing, drop = F]))
        }
      }
    },
    
    # COMPUTATION ================
    
    #' Evaluate data in peak table
    reevaluate_peak_table = function() {
      "reevalutes the peak table (not currently implemented)"
      warning("peak table evaluation not implemented for this class")
    },
   
    # PLOTTING ===================
    
    #' Plot the data points in the peak table
    #' @param y = expression which data to plot (will be evaluated in context of the data frame)
    #' @param ylab = y axis label
    #' @param title = title of the plot
    #' @param data = peak table data (by default the whole peak table)
    plot_data_table = function(y = NULL, ylab = "", title = "", data = get_data_table()) {
      "Plot the data points in the peak table
      
      #' @param y = expression which data to plot (will be evaluated in context of the data frame)
      
      #' @param ylab = y axis label
       
      #' @param title = title of the plot
       
      #' @param data = peak table data (by default the whole peak table)
      "
      compounds <- sapply(data[[dataTableKeys['name']]], 
                          function(i) if (nchar(i) > 0 && i != " - ") paste(i, "\n") else "")
      data$.labels = paste0(compounds, "RT: ", data[[dataTableKeys['rt']]], " (#", data[[dataTableKeys['peak_nr']]], ")")
      data$.x <- 1:nrow(data)
      
      # aesthetics:
      mapping <- structure(ggplot2:::rename_aes(list(x = quote(.x), y = substitute(y))), class = "uneval")
      
      # plot
      ggplot2:::ggplot(data, mapping) + 
        geom_point(size=3, shape=21, fill="gray", color="black") + 
        scale_x_continuous(breaks=data$.x, labels=data$.labels) +
        labs(title = title, y = ylab, x = "") + theme_bw() + 
        theme(legend.position="bottom", axis.text.x = element_text(angle = 60, hjust = 1)) 
    },
    
    #' Make a ggplot of the references in the peak table
    plot_refs = function(y, ylab = "", title = "references") {
      if (missing(y))
        stop("Need to pass in what to plot on the y axis!")
      "plot the data of the reference peaks, see \\code{plot_data_table} for details on syntax"
      do.call(.self$plot_data_table, list(y = substitute(y), ylab = ylab, title = title, data = get_data_table(type = "ref")))
    },
    
    #' Make a ggplot of the data peaks in the peak table
    plot_data = function(y, ylab = "", title = "data peaks") {
      "plot the data of the actual sample peaks, see \\code{plot_data_table} for details on syntax"
      if (missing(y))
        stop("Need to pass in what to plot on the y axis!")
      do.call(.self$plot_data_table, list(y = substitute(y), ylab = ylab, title = title, data = get_data_table(type = "data")))
    },
    
    #' Plot the data (both masses and ratios) - much faster than ggplot but not as versatile
    #' 
    #' @aliases isoplot
    #' @param tlim time range, should be in the same tunits
    #' @param masses which masses to plot (all defined in plot optinos by default)
    #' @param ratios which ratios to plot (all defined in plot options by default)
    #' @param tunits time units, as defined in tunits (currently either "s" or "min"), takes the one set in plotOptions as default
    #' @note consider adding a ... option to allow setting plot options
    #' or temporarily overwriting them here
    plot = function(tlim = NULL, mass_ylim = NULL, ratio_ylim = NULL,
                    masses = names(.self$plotOptions$masses),
                    ratios = names(.self$plotOptions$ratios),
                    tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]]) {
      "Plot the data (both masses and ratios) - much faster than ggplot but not as versatile
      
      #' @param tlim time range, should be in the same tunits
      
    #' @param masses which masses to plot (all defined in plot optinos by default)
     
    #' @param ratios which ratios to plot (all defined in plot options by default)
     
    #' @param tunits time units, as defined in tunits (currently either 's' or 'min'), takes the one set in plotOptions as default      
      "
      if (!is.null(ratios) && !is.null(masses))
        layout(matrix(c(1,2), byrow=TRUE, ncol=1))
      if (!is.null(ratios))
        plot_ratios(tlim = tlim, ylim = ratio_ylim, ratios = ratios, tunits = tunits)
      if (!is.null(masses))
        plot_masses(tlim = tlim, ylim = mass_ylim, masses = masses, tunits = tunits)
    },
    
    #' Plot the masses (this if much faster than ggplot but not as versatile)
    #' @param tlim -> time range, should be in the same tunits
    #' @param tunits -> time units, as defined in tunits (currently either "s" or "min"), takes the one set in plotOptions as default
    plot_masses = function(tlim = NULL, ylim = NULL, 
                           masses = names(.self$plotOptions$masses),
                           tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]]) {
      "Plot the masses (this if much faster than ggplot but not as versatile)"
      
      # get data
      time <- paste0("time.", tunits)
      data <- get_mass_data(masses = masses)
      data$time.plot <- data[[time]] # which time axis to plot
      
      # range and limits
      if (is.null(tlim))
        tlim = c(min(data$time.plot), max(data$time.plot)) 
      data <- subset(data, time.plot >= tlim[1] & time.plot <= tlim[2])
      if (nrow(data) == 0)
        stop("with the select time range (", paste0(tlim, collapse=", "), tunits, "), there is no data left to plot")
      if (is.null(ylim)) {
        # ylimits from all traces
        ymin <- min(sapply(paste0(masses, ".offset"), function(mass) min(data[[mass]])))
        ymax <- max(sapply(paste0(masses, ".offset"), function(mass) max(data[[mass]])))
        ylim = c(ymin, ymax)
      }
      
      # plot
      graphics:::plot(0,0, type="l", xlim=tlim, ylim=ylim, 
                      xlab=paste(plotOptions$labels$x, " [", tunits, "]", sep=""), 
                      ylab=plotOptions$labels$ymasses,
                      xaxs = "i", yaxs = "r")
      
      # print masses
      sapply(masses, function(mass)
          lines(data$time.plot, data[[paste0(mass, ".offset")]], 
                col = plotOptions$masses[[mass]]$color))
      invisible(NULL)
    },
    
    #' Plot the ratios (this if much faster than ggplot but not as versatile)
    #' @param tlim -> time range, should be in the same tunits
    #' @param tunits -> time units, as defined in tunits (currently either "s" or "min"), takes the one set in plotOptions as default
    plot_ratios = function(tlim = NULL, ylim = NULL, 
                           ratios = names(.self$plotOptions$ratios),
                           tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]]) {
      "Plot the ratios (this if much faster than ggplot but not as versatile)"
      
      # get data
      time <- paste0("time.", tunits)
      data <- get_ratio_data(ratios = ratios)
      data$time.plot <- data[[time]] # which time axis to plot
      
      # range and limits
      if (is.null(tlim))
        tlim = c(min(data$time.plot), max(data$time.plot)) 
      data <- subset(data, time.plot >= tlim[1] & time.plot <= tlim[2])
      if (nrow(data) == 0)
        stop("with the select time range (", paste0(tlim, collapse=", "), tunits, "), there is no data left to plot")
      if (is.null(ylim)) {
        # ylimits from all traces
        ymin <- min(sapply(ratios, function(ratio) min(data[[ratio]])))
        ymax <- max(sapply(ratios, function(ratio) max(data[[ratio]])))
        ylim = c(ymin, ymax)
      }
      
      # plot
      graphics:::plot(0,0, type="l", xlim=tlim, ylim=ylim, 
                      xlab=paste(plotOptions$labels$x, " [", tunits, "]", sep=""), 
                      ylab=plotOptions$labels$yratios, 
                      xaxs = "i", yaxs = "r")
      
      # print ratios
      sapply(ratios, function(ratio)
        lines(data$time.plot, data[[ratio]], 
              col = plotOptions$ratio[[ratio]]$color))
      invisible(NULL)
    },
    
    #' ggplot the data
    #' @param tlim time range (in tunits units)
    #' @param tunits units (currently "s" or "min")
    #' @param masses vector of the masses to plot (if NULL, panel excluded)
    #' @param ratios vector of the ratios to plot (if NULL, panel excluded)
    make_ggplot = function(tlim = NULL, tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]],
                      masses = names(.self$plotOptions$masses),
                      ratios = names(.self$plotOptions$ratios)) {
      "ggplot the data

    #' @param tlim time range (in tunits units)
    
    #' @param tunits units (currently 's' or 'min')
     
    #' @param masses vector of the masses to plot (if NULL, panel excluded)
    
    #' @param ratios vector of the ratios to plot (if NULL, panel excluded)
      "
      
      library(ggplot2)
      
      # checks
      if (is.null(masses) && is.null(ratios))
        stop("error, no masses or ratios provided, need at least one set of data")
      
      # get data
      time <- paste0("time.", tunits)
      mass_data <- ratio_data <- data.frame()
      if (!is.null(masses))
        mass_data <- mutate(get_mass_data(masses = masses, melt = T), 
                            panel = plotOptions$labels$ymasses)
      if (!is.null(ratios))
        ratio_data <- mutate(get_ratio_data(ratios = ratios, melt = T), 
                             panel = plotOptions$labels$yratios, signal.offset = signal)
      
      # adjust time range (this is better to do here instead of with xylim() so the panels' y scales get adjusted properly)
      data <- rbind(ratio_data, mass_data)
      data$time.plot <- data[[time]] # which time axis to plot
      if (!is.null(tlim))
        data <- subset(data, time.plot >= tlim[1] & time.plot <= tlim[2])
      
      # trace labels
      traces <- c(lapply(plotOptions$ratios[ratios], function(i) i$label),
                  lapply(plotOptions$masses[masses], function(i) i$label))
      data$variableF <- factor(data$variable, levels = names(traces), labels = traces)
            
      # plot
      p <- ggplot(data, aes_string(x = time, y = "signal.offset", colour = "variableF")) +
        geom_line() + theme_bw() +
        scale_x_continuous(expand = c(0,0)) + 
        labs(x = paste0(plotOptions$labels$x, " [", tunits, "]"), y = "", colour = "Trace")
      
      # plot numbers and references (add to normal plot and introduce as plotOption)
      if (!is.null(table <- get_data_table()) && nrow(table) > 0) {
        table$.label <- paste0(table[[dataTableKeys['peak_nr']]], ifelse(table[[dataTableKeys['ref_peak']]], "*", ""))      
        
        p <- p + geom_text(data = mutate(table, .y = 0, panel = plotOptions$labels$ymasses), 
                ggplot2::aes_string(x = dataTableKeys['rt'], y = ".y", label = ".label", colour = NULL), size=6, show.legend = F)
      }
      
      if (is.null(masses))
        return (p + labs(y = plotOptions$labels$yratios))
      else if (is.null(ratios))
        return (p + labs(y = plotOptions$labels$ymasses))
      else
        return (p + facet_grid(panel~., scales="free")) # panels
    },
  
    # DATA EXPORT ==============
    #' summarize data to pdf
    #' @param file the file name where to save, by default saves where the file was original from
    #' @param whether to try to compact the pdf to make it smaller
    #' @param plots whether to include the plot
    #' @param table whether to include the table
    summarize = function(file = default_filename(), folder = .self$filepath, 
                         width = 16, height = 12, plots = TRUE, table = TRUE,
                         compact = TRUE, ...) {
      # these are only in the 'suggests' list so need to make sure to require them
      library(ggplot2)
      library(gridExtra)
      
      default_filename <- function() {
        file.path(folder, paste0("summary_", .self$filename, ".pdf"))
      }
      
      message("Creating summary for ", .self$filename, " ...")
      
      # saving to pdf
      pdf(file, width=width, height=height)
      
      # data table (split in two)
      if (table) {
        cols_per_row <- ceiling((ncol(dataTable) + 4)/2)
        g1 <- gridExtra::tableGrob(
          get_data_table()[1:cols_per_row], 
          theme = gridExtra::ttheme_minimal(), 
          rows = NULL) 
#           show.rownames=FALSE, gpar.coretext = grid::gpar(fontsize=10), 
#           gpar.coltext = grid::gpar(fontsize=12, fontface="bold"), 
#           gpar.colfill = grid::gpar(fill=NA,col=NA), 
#           gpar.rowfill = grid::gpar(fill=NA,col=NA), h.even.alpha = 0)
        g2 <- gridExtra::tableGrob(
          cbind(get_data_table()[dataTableKeys[c("peak_nr", "ref_peak", "rt", "name")]],
                get_data_table()[(1+cols_per_row):ncol(dataTable)]), 
          theme = gridExtra::ttheme_minimal(), 
          rows = NULL) 
        
        # combine the whole ting
        grid.arrange(g1, g2, ncol=1,
                  top=paste0("\nIsodat binary read of ", filename, " (analyzed on ", creation_date, ")"))
                            # NOTE: these are specific to the hydrogen case, implement more universally!
                            #  ", H3 factor: ", round(data$`H3 factor`, digits=4),
                            #  "\nGC: ", data$GCprogram, 
                            #  " // AS: ", data$ASprogram, 
                            #  " // Method: ", data$MSprogram))
        
      }
      
      # plots (data, referenes, peak table)
      if (plots) {
        grid::grid.newpage()
        grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))
        print(make_ggplot() + 
                labs(title = paste0("Isodat binary read of ", filename, " (analyzed on ", creation_date, ")")), 
              vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(plot_refs(), vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(plot_data(), vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 2))
      }
      
      dev.off()
      message("Summary saved to ", file)
      
      # try to compact (requires ghostscript)
      if (compact) {
        tryCatch( {
          # this is essentially what tools::compactPDF(file, gs_quality = "screen") does but always does the conversion (not just if it's 10% smaller)
          tf <- tempfile("pdf")
          res <- 0
          res <- system2("gs", c("-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite", 
                            "-dPDFSETTINGS=/screen", 
                            "-dCompatibilityLevel=1.5", 
                            "-dAutoRotatePages=/None", 
                            sprintf("-sOutputFile=%s", tf), 
                            sprintf("%s", gsub("\\s", "\\\\ ", file))), FALSE, FALSE)
          if (!res && file.exists(tf)) {
            old <- file.info(file)$size
            new <- file.info(tf)$size
            if (new < old && new/old > 0.5) { # somethign went wrong if the file is less than 50 percent
              file.copy(tf, file, overwrite = TRUE)
              message("File successfully compacted by ", round(100 * (1-new/old)), "%")
            }
          }
          unlink(tf)
        },
        warning = function(w) {}, error = function(e) {})
      }
    },
    
    
    #' export data (by default to csv)
    export_data = function(file = default_filename(), type = c("table", "chrom"), extension = "csv", sep = ",", headers = TRUE, ...) {
      # what to export
      if (missing(type)) type <- "table"
      type <- match.arg(type)
      
      # default filename
      default_filename <- function() {
        file.path(.self$filepath, paste0("export_", .self$filename, ".", extension))
      }
      
      message("Creating ", type, " export for ", .self$filename, " ...")
      if (type == "table")
        write.table(get_data_table(), file, sep = sep, row.names = FALSE, col.names = headers, ...)
      else if (type == "chrom")
        write.table(massData, file, sep = sep, row.names = FALSE, col.names = headers, ...)
      message(type, " data exported to ", file)
    }
    
  ),
)