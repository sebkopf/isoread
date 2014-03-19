#' @include IsodatFileClass.R
#' @include IrmsContinuousFlowDataClass.R
NULL

#' Isodat file class
#' @exportClass IsodatHydrogenContinuousFlowFile
IsodatHydrogenContinuousFlowFile <- setRefClass(
  "IsodatHydrogenContinuousFlowFile",
  contains = c("IsodatFile", "IrmsContinuousFlowData"),
  fields = list (),
  methods = list(
    #' initialize
    initialize = function(...) {
      callSuper(...)
      initDefaultPlotOptions()
    },
    
    #' Set the default plot options
    initDefaultPlotOptions = function(){
      callSuper(
        masses = list(
          mass2 = list(label = "Mass 2", color="black", offset=200), #offset in mV
          mass3 = list(label = "Mass 3", color="dark green", offset=0)),
        ratios = list ( 
          ratio_3o2 = list(label = "Ratio", color="black", offset=0))
        )
    },
    
    #' expand parent load function with key_cleanup
    load = function(...) {
      callSuper(...)
      clean_keys(
        removeText="Arial", 
        removePattern = "[&{}!^@?#]", 
        unlessByteLength = 26, 
        unlessText = "Is Ref.?")
    }, 
    
    #' expand parent procdess function specifically for hydrogen continuous flow data
    #' @param readChromData whether to read chromatographic mass+ratio data (can be a lot of data)
    process = function(readChromData = TRUE, ...) {
      callSuper()
      
      # process header
      move_to_key("CRawDataScanStorage")
      parse("UTF16", length = 13, id = "data_trace_name", skip_first = 14)
      parse("long", id = "n_measurements", skip_first = 20)
      parse("short", id = "n_ions", skip_first = 0)
      parse("short", id = "n_ions", skip_first = 29) 
      
      # read mass2/mass3 data trace
      if (readChromData) {
        parse_array(
          types = c(time = "float", mass2 = "double", mass3 = "double"), 
          n = data$n_measurements, id = "mass", skip_first = 0)
      }
      
      # footer      
      parse("UTF16", length=6, id = "trace1_name", skip_first = 70)
      parse("UTF16", length=6, id = "trace2_name", skip_first = 4)
      
      # ratio data header
      move_to_key("CRatioDataScanStorage")
      parse("UTF16", length = 13, id = "data_ratio_name", skip_first = 14)
      parse("long", id = "n_ratio_measurements", skip_first = 20)
      parse("short", id = "n_ratios", skip_first = 0)
      parse("short", id = "n_ratios", skip_first = 18)
      
      # read ratio data
      if (readChromData) {
        parse_array(
          types = c(time = "float", ratio_3o2 = "double"), 
          n = data$n_ratio_measurements, id = "ratio", skip_first = 0)
      }
      
      # other information
      move_to_key("H3 Factor")
      parse("double", id = "H3factor", skip_first = 8)
      data$GCprogram <<- find_key(".gcm$")
      data$MSprogram <<- find_key(".met$")
      data$Filename <<- find_key(".cf$")
      data$ASprogram <<- find_key("Internal")
      
      # reorganize data to fit 
      if (readChromData) {
        chromData <<- cbind(data$mass, data$ratio['ratio_3o2'])
        data$mass <<- data$ratio <<- NULL
      }
    },
    
    #' custom show function to display roughly what data we've got going
    show = function() {
      cat("\nShowing summary of", class(.self), "\n")
      callSuper()
      cat("\n\nData (first couple of rows):\n")
      print(head(chromData))
      cat("\nPeak Table:\n")
      print(peakTable)
      cat("\nOther information:\n")
      print(sapply(data, identity, simplify=T))
    }
  )
)