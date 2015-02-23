#' @include IsodatFileClass.R
#' @include IrmsContinuousFlowDataClass.R
NULL


#' Continous Flow DataClass
#' 
#' Objects of this class hold the isotopic data from continuous flow information
#' recorded in Isodat files (.dxf files) currently only tested with Isodat 3.0 files).
#' 
#' This class is derived from \link{IrmsContinuousFlowData} which defines a number
#' of useful plotting, export and data access methods. This class also derived
#' \link{BinaryFile} which provides functionality for interacting with the
#' underlying \link{IsodatFile}.
#' 
#' @name IsodatContinuousFlowFile
#' @exportClass IsodatContinuousFlowFile
#' @seealso \link{BinaryFile}, \link{IsodatFile}, \link{IrmsContinuousFlowData}, \link{IrmsData}
IsodatContinuousFlowFile <- setRefClass(
  "IsodatContinuousFlowFile",
  contains = c("IsodatFile", "IrmsContinuousFlowData"),
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
      # overwrite in derived classes and set data table definitions properly for more specialized behaviour
      # see IrmsContinuousFlowDataClass for details on requirements and functionality
    },
    
    # READ DATA =========================
    
   
    
    cleanup = function(clean_chrom_data = FALSE, ...) {
      callSuper(...)
      if (clean_chrom_data)
        massData <<- data.frame()
    },
    
    # DISPLAY =============================
    
    get_info = function(show = c()) {
      info <- rbind(callSuper(), data.frame(Property = "Detected peaks", Value = as.character(nrow(dataTable))))
      if (length(show) == 0)
        info
      else
        info[na.omit(match(show, info$Property)),]
    },
    
    #' custom show function to display roughly what data we've got going
    show = function() {
      cat("\nShowing summary of", class(.self), "\n")
      callSuper()
      cat("\n\nData (first couple of rows):\n")
      print(head(get_mass_data()))
      cat("\n\nData table:\n")
      print(get_data_table())
    }
    
  ) 
)