#' @include IsodatFileClass.R
#' @include IrmsDualInletDataClass.R
NULL

#' Clumped dual inlet data class
#' 
#' 
#' @name IsodatClumpedDataFile
#' @exportClass IsodatClumpedDataFile
#' @seealso \link{BinaryFile}, \link{IsodatFile}, \link{IrmsDualInletData}, \link{IrmsData}
IsodatClumpedDataFile <- setRefClass(
  "IsodatClumpedDataFile",
  contains = c("IsodatFile", "IrmsDualInletData"),
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
    
    
    #' expand parent procdess function specifically for hydrogen continuous flow data
    #' @param readChromData whether to read chromatographic mass+ratio data (can be a lot of data)
    process = function(readChromData = TRUE, ...) {
      callSuper()
      
      #     Adding implementation to find d13C, d18O calculated by isodat
      move_to_key("CDualInletEvaluatedData")
      parse("double", length = 1, id = "d13C", skip_first = 788)
      
      move_to_key("CDualInletEvaluatedData")
      parse("double", length = 1, id = "d18O", skip_first = 1061)
      
      
      
      move_to_key("Standard Pre")
      parse("double", length = 6, id = "standard_pre", skip_first = 64)
      
      for (i in 1:7) {
        move_to_key(paste("Standard", i-1))
        parse("double", length = 6, id = paste0("standard_", i), skip_first = if (i == 1) 82 else 64)
        move_to_key(paste("Sample", i-1))
        parse("double", length = 6, id = paste0("sample_", i), skip_first = 64)
      }
      
      data$standards <<- as.data.frame(data[grepl("standard", names(data))])
      data$samples <<- as.data.frame(data[grepl("sample", names(data))])
      

      #adding implementation to find background, pressure adjust, and peak center strings
      # not parsing them to numbers right now, but I will
      
      #move_to_key("Background:")
      #parse("UTF32",length=1,id="background", skip_first=13)
      #print(data$background)
      
      # ggplot(all_data, aes(x = sub("\\w+_(\\w+)", "\\1", variable), value, colour = sub("\\w+_(\\w+)", "\\1", variable), shape = sub("(\\w+)_\\w+", "\\1", variable))) + geom_point(size = 3) + facet_wrap(~mass, scales = "free") + labs(colour = "cycle")
      
#       # process header
#       move_to_key("CRawDataStandardBlock")
#       parse("UTF16", length = 13, id = "data_trace_name", skip_first = 14)
#       parse("long", id = "n_measurements", skip_first = 20)
#       parse("short", id = "n_ions", skip_first = 0)
#       parse("short", id = "n_ions", skip_first = 29) 
#       
#       # read mass2/mass3 data trace
#       if (readChromData) {
#         parse_array(
#           types = c(time = "float", mass2 = "double", mass3 = "double"), 
#           n = data$n_measurements, id = "mass", skip_first = 0)
#       }
#       
#       # footer      
#       parse("UTF16", length=6, id = "trace1_name", skip_first = 70)
#       parse("UTF16", length=6, id = "trace2_name", skip_first = 4)
#       
#       # ratio data header
#       move_to_key("CRatioDataScanStorage")
#       parse("UTF16", length = 13, id = "data_ratio_name", skip_first = 14)
#       parse("long", id = "n_ratio_measurements", skip_first = 20)
#       parse("short", id = "n_ratios", skip_first = 0)
#       parse("short", id = "n_ratios", skip_first = 18)
#       
#       # read ratio data
#       if (readChromData) {
#         parse_array(
#           types = c(time = "float", ratio_3o2 = "double"), 
#           n = data$n_ratio_measurements, id = "ratio", skip_first = 0)
#       }
#       
#       # other information
#       move_to_key("H3 Factor")
#       parse("double", id = "H3factor", skip_first = 8)
#       data$GCprogram <<- find_key(".gcm$")
#       data$MSprogram <<- find_key(".met$")
#       data$Filename <<- find_key(".cf$")
#       data$ASprogram <<- find_key("Internal")
#       
#       # reorganize data, move to IrmsDataClass structure
#       if (readChromData) {
#         chromData <<- cbind(data$mass, data$ratio['ratio_3o2'])
#         data$mass <<- data$ratio <<- NULL
#       }
#       
#       # peak table (FIXME: this could use some refactoring)      
#       rawtable <- rawdata[subset(keys, value=="CPkDataListBox")$byteEnd:subset(keys, value=="CGCPeakList")$byteStart]
#       arials <- grepRaw("([Arial][^\u0020-\u007e]){5}", rawtable, all=TRUE)
#       #FIXME: newer versions of isodat (2.5 and 3.1 don't have this business, just 18 bytes between each label!)
#       if (length(arials) < 5) {
#         warning("peak table entries not found, this could be because the file might be created with a newer version (>2) of isodat. ",
#                 "files from isodat 2.5 and 3.1 are known to have this problem but are currently not yet supported")
#       } else {
#         entries<-NULL
#         spos <- 9 + (regexpr("14000000fffeff08", paste(readBin(rawtable[1:(arials[1]-48)], "raw", n=(arials[1]-48)), collapse=""), fixed=TRUE)-1)/2
#         for (i in arials) {
#           epos<-(i-48)
#           entries<-c(entries, paste(readBin(rawtable[spos:epos], "character", n=(epos-spos)/2, size=2), collapse=""))
#           spos<-i+100
#         }
#         entries <- entries[-length(entries)] # last entry is garbage
#         
#         
#         # sometimes there is an extra column (rps), sometimes not
#         if (! (rps_column <- length(entries) %% 28 == 0) && # rps column
#               ! (length(entries) %% 27 == 0)) { # no rps column
#           # neither 27 nor 28 columns! not sure what's going on
#           assign("isoread_debug", entries, env = globalenv())
#           warning("it appears the peak table has neither exactly 27 nor 28 columns, not sure how to deal with this scenario. ",
#                   "a dump of all entries recovered from the peak table is stored in the global variable 'isoread_debug'")
#         } else {
#           table<-matrix(entries, byrow=TRUE, ncol=if(rps_column) 28 else 27) # FIXME not sure this is always true that it's 27 columns but appears to be the case
#           df<-data.frame(table[2:nrow(table),], stringsAsFactors=FALSE)
#           names(df)<-table[1,]
#           
#           # add rps column if missing
#           if (!rps_column)
#             df$`Rps 3H2/2H2` <- ""
#           
#           # process peak nr
#           if ( !('Peak Nr.' %in% names(df) ))
#             stop("'Peak Nr.' column not found in peak table. Only available columns are ", paste0(names(df), collapse = ", "))
#           
#           peakNrPattern <- "^([0-9]+)([\\*\\+]?)$"
#           df <- mutate(
#             df,
#             `Ref. Peak` = sub(peakNrPattern, '\\2', `Peak Nr.`) == "*", # whether it is a reference peak
#             Status = sapply(sub(peakNrPattern, '\\2', `Peak Nr.`), # whether peak was added
#                             function(x) { if (x=="+") "Added" else "Auto" }),
#             Formula = "",
#             `Peak Nr.` = as.integer(sub(peakNrPattern, '\\1', `Peak Nr.`))) # peak number as integer
#           
#           # store peak table data
#           peakTable <<- df
#         }
#       } 
#       
    },
      
    #' custom show function to display roughly what data we've got going
    show = function() {
      cat("\nShowing summary of", class(.self), "\n")
      callSuper()
      #cat("\n\nData (first couple of rows):\n")
      #print(head(chromData))
      #cat("\nPeak Table:\n")
      #print(peakTable)
    }
  )
)