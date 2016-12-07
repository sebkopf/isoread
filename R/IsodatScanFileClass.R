#' @include IsodatFileClass.R
#' @include IrmsScanDataClass.R
NULL

#' Scan data class
#' 
#' @name IsodatScanFile
#' @exportClass IsodatScanFile
#' @seealso \link{BinaryFile}, \link{IsodatFile}, \link{IrmsScanData}, \link{IrmsData}
IsodatScanFile <- setRefClass(
  "IsodatScanFile",
  contains = c("IsodatFile", "IrmsScanData"),
  fields = list (),
  methods = list(
    #' initialize
    initialize = function(...) {
      callSuper(...)
      init_irms_data()
    },
    
    #' initialize irms data container
    init_irms_data = function(){
      callSuper()      
    },
    
    # READ DATA =========================
    
    #' expand process function specifically for dual inlet type data
    process = function(...) {
      callSuper()
      
      # find recorded masses
      masses <- find_key(
        "Mass \\s+\\d+\\.\\d+ \\[C\\d+\\]",
        byte_min = find_key("CScanStorage", occ = 1, fix = T)$byteEnd,
        byte_max = find_key("CPlotRange", occ = 1, fix = T)$byteStart)$value
      
      if (length(masses) == 0)
        stop("Error: no keys named 'Mass ..' found. Cannot identify recorded mass traces in this file.", call. = FALSE)
      
      # unless mass plot options are already manually defined (in init_irms_data), define them automatically here and assign colors
      mass_names <- sub("Mass \\s+(\\d+).*", "mass\\1", masses)
      if (length(plotOptions$masses) == 0) {
        # color blind friendly pallete (9 colors)
        palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                     "#D55E00", "#0072B2", "#CC79A7", "#999999", "#F0E442")
        if (length(masses) > length(palette))
          stop("Currently only supporting up to ", length(palette), 
               " automatically assigned different colors for masses but ",
               "this file is recording data for ", length(masses), 
               " different masses. Please define the plotOptions manually.", call. = FALSE)        
        set_plot_options(
          masses = setNames(
            sapply(seq_along(masses), function(i) list(list(label = masses[i], 
                                                            color = palette[i]))), 
            mass_names)
        )
      }
      
      # go to beginning of data array
      move_to_key("CScanStorage") # start of data
      skip(34) # bunch of x00s
      n <- parse("short", id = "n_steps") # the number of data points
      skip(35) # move to beginning of array (skip CBinary and x00 s)
      end <- find_key("CPlotInfo", fixed = T)$byteStart # end point of data arra
      n_apparent <- (end - pos - 24) / (4 + length(masses) * 8) # apparent n
      
      # consistency check
      if (n_apparent != n) {
        # there was a problem here, proceed with caution (and floored n value)
        n <- floor(n_apparent)
        stop(sprintf(
          "Something doesn't quite add up in this file, the number of data points was identified as %.0f but the array size indicates %.3f. Trying to read the file with %.0f. Please proceed with caution.", n_true, n_apparent, n))
      }
      
      # parse array
      invisible(parse_array(
        types = c(step = "float", 
                  sapply(mass_names, function(i) list("double"))), 
        n = n, id = "mass"))
      massData <<- data$mass
      data$mass <<- NULL
    },
    
    # EXPORT ===================
    
    #' custom show function to display roughly what data we've got going
    show = function() {
      cat("\nShowing summary of", class(.self), "\n")
      callSuper()
      cat("\n\nMass data (first 5 rows):\n")
      print(head(get_mass_data()))
    }
  )
)