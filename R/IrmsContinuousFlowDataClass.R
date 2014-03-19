#' @include IrmsDataClass.R
NULL

#' IrmsContinuousFlowData reference class
#' @exportClass IrmsContinuousFlowData
IrmsContinousFlowData <- setRefClass(
  "IrmsContinuousFlowData",
  contains = "IrmsData",
  fields = list (
    chromData = 'data.frame', # chromatographic data
    peakTable = 'data.frame' # table of peaks
    ),
  methods = list(
    #' sets default plot options
    initDefaultPlotOptions = function(...) {
      callSuper(
          tunits = list(value = 1, labels = c("s", "min"), funcs = c(function(x) x, function(x) x/60)),
          labels = list(x="Time", ymasses = "Signal [mV]", yratios = "Ratio"),
          masses = list(), # example entry: mass1 = list(label = "Mass 1", color="black", offset=200) # offset in mV
          ratios = list (), # example entry: ratio1 = list(label = "Ratio 1", color="red")), #offset in dimensionells ratio units
          baseMarker = list(on = TRUE, color="red"),
          apexMarker = list(on = TRUE, color="red"),
          edgeMarker = list(on = TRUE, color="blue"))
      setPlotOptions(...)
    },
    
    #' get data for masses
    #' @param masses which masses to retrieve, all defined ones by defaul
    #' @param melt whether to melt the data frame
    #' @note consider storing the extra x units in the chromData but worries about the size of this
    #' object keep me from storing extra information in chromData
    get_mass_data = function(masses = names(.self$plotOptions$masses), melt = FALSE) {
      # checks
      if (length(missing <- setdiff(masses, names(.self$plotOptions$masses))) > 0)
        stop("Some mass traces ('", paste(missing, collapse = ", ") ,"') are not defined in plotOptions$masses.")
      
      if (length(missing <- setdiff(masses, names(chromData))) > 0)
        stop("Some mass traces ('", paste(missing, collapse = ", ") ,"') are not available from the chromatographic data.")

      # data
      data <- subset(chromData, select = c("time", masses))
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
          measure = masses, value.name = "signal")
        offset.melt <- melt(subset(data, select = offsets), value.name = "signal.offset")
        return (cbind(mass.melt, offset.melt["signal.offset"]))
      } else
        return(data)
    },
    
    #' get data for ratios
    #' @param ratios which ratios to retrieve, all defined ones by default
    #' @param melt whether to melt the data frame
    get_ratio_data = function(ratios = names(.self$plotOptions$ratios), melt = FALSE) {
      # checks
      if (length(missing <- setdiff(ratios, names(.self$plotOptions$ratios))) > 0)
        stop("Some ratio traces ('", paste(missing, collapse = ", ") ,"') are not defined in plotOptions$ratios.")
      
      if (length(missing <- setdiff(ratios, names(chromData))) > 0)
        stop("Some ratio traces ('", paste(missing, collapse = ", ") ,"') are not available from the chromatographic data.")
      
      # data
      data <- subset(chromData, select = c("time", ratios))
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
    
    #' Plot the data (both masses and ratios) - much faster than ggplot but not as versatile
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
      layout(matrix(c(1,2), byrow=TRUE, ncol=1))
      plot_ratios(tlim = tlim, ylim = ratio_ylim, ratios = ratios, tunits = tunits)
      plot_masses(tlim = tlim, ylim = mass_ylim, masses = masses, tunits = tunits)
    },
    
    #' Plot the masses (this if much faster than ggplot but not as versatile)
    #' @param tlim -> time range, should be in the same tunits
    #' @param tunits -> time units, as defined in tunits (currently either "s" or "min"), takes the one set in plotOptions as default
    plot_masses = function(tlim = NULL, ylim = NULL, 
                           masses = names(.self$plotOptions$masses),
                           tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]]) {
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
    },
    
    #' Plot the ratios (this if much faster than ggplot but not as versatile)
    #' @param tlim -> time range, should be in the same tunits
    #' @param tunits -> time units, as defined in tunits (currently either "s" or "min"), takes the one set in plotOptions as default
    plot_ratios = function(tlim = NULL, ylim = NULL, 
                           ratios = names(.self$plotOptions$ratios),
                           tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]]) {
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
    },
    
    #' ggplot the data
    #' @param tlim time range (in tunits units)
    #' @param tunits units (currently "s" or "min")
    #' @param masses vector of the masses to plot (if NULL, panel excluded)
    #' @param ratios vector of the ratios to plot (if NULL, panel excluded)
    ggplot = function(tlim = NULL, tunits = .self$plotOptions$tunits$labels[[.self$plotOptions$tunits$value]],
                      masses = names(.self$plotOptions$masses),
                      ratios = names(.self$plotOptions$ratios)) {
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
      p <- ggplot2:::ggplot(data, aes_string(x = time, y = "signal.offset", colour = "variableF")) +
        geom_line() + theme_bw() +
        scale_x_continuous(expand = c(0,0)) + 
        labs(x = paste0(plotOptions$labels$x, " [", tunits, "]"), y = "", colour = "Trace")
      
      if (is.null(masses))
        return (p + labs(y = plotOptions$labels$yratios))
      else if (is.null(ratios))
        return (p + labs(y = plotOptions$labels$ymasses))
      else
        return (p + facet_grid(panel~., scales="free")) # panels
    }
    
    )
)