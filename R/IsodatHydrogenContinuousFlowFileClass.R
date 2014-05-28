#' @include IsodatFileClass.R
#' @include IrmsContinuousFlowDataClass.R
NULL

#' H-CSIA DataClass
#' 
#' Objects of this class hold the isotopic data from compound specific hydrogen
#' isotope analysis recorded in Isodat file formats (currently supported isodat
#' version is 2.0 for chromatographic and peak table data and isodat version 2.5 and
#' 3.0 for chromatographic data only).
#' 
#' This class is derived from \link{IrmsContinuousFlowData} which defines a number
#' of useful plotting, export and data access methods. This class also derived
#' \link{BinaryFile} which provides functionality for interacting with the
#' underlying \link{IsodatFile}.
#' 
#' @name IsodatHydrogenContinuousFlowFile
#' @exportClass IsodatHydrogenContinuousFlowFile
#' @seealso \link{BinaryFile}, \link{IsodatFile}, \link{IrmsContinuousFlowData}, \link{IrmsData}
IsodatHydrogenContinuousFlowFile <- setRefClass(
  "IsodatHydrogenContinuousFlowFile",
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
      
      # mass and ratio plot options
      set_plot_options(
        masses = list(
          mass2 = list(label = "Mass 2", color="black", offset=200), #offset in mV
          mass3 = list(label = "Mass 3", color="dark green", offset=0)),
        ratios = list ( 
          ratio_3o2 = list(label = "Ratio", color="black", offset=0))
        )
      
      # peak table definition
      peakTableColumns <<- data.frame(
        data = c('Filename', 'Peak Nr.', 'Ref. Peak', 'Status', 'Component', 'Formula', 'Master Peak', 'Ref. Name', 
                 'Start\n[s]', 'Rt\n[s]', 'End\n[s]', 'Width\n[s]', 'Ampl. 2\n[mV]', 'Ampl. 3\n[mV]', 'BGD 2\n[mV]', 'BGD 3\n[mV]', 
                 'Area All\n[Vs]', 'Area 2\n[Vs]', 'Area 3\n[Vs]', 'rArea All\n[mVs]', 'rArea 2\n[mVs]', 'rArea 3\n[mVs]', 
                 'R 3H2/2H2\n', 'rR 3H2/2H2\n', 'rd 3H2/2H2\n[per mil]\nvs. methane ref', 'd 3H2/2H2\n[per mil]\nvs. VSMOW', 
                 'DeltaDelta 3H2/2H2\n[0 ]', 'R 2H/1H', 'd 2H/1H\n[per mil]\nvs. VSMOW', 'AT% 2H/1H\n[%]', 'Rps 3H2/2H2'),
        type = c("character", "integer", "logical", "character", "character", "character", "character", "character", 
                 rep("numeric", 14),
                 "Ratio", "Ratio", "Delta", "Delta", "numeric", "Ratio", "Delta", "numeric", 'numeric'),
        show = TRUE, stringsAsFactors = FALSE)
      peakTableColumns <<- mutate(
        peakTableColumns,
        column = sub('^([^\n]+).*', '\\1', data), # pull out column names from the data names
        units = sub('\n', ' ', sub('^[^\n]+(\n)?(.*)$', '\\2', data))) # pull out units from the data names
      
      # key peaks
      peakTableKeys <<- c(peak_nr = "Peak Nr.", ref_peak = "Ref. Peak", 
                          rt = "Rt", rt_start = "Start", rt_end = "End",
                          name = "Component")
    },
    
    # READ DATA =========================
    
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
      
      # reorganize data, move to IrmsDataClass structure
      if (readChromData) {
        chromData <<- cbind(data$mass, data$ratio['ratio_3o2'])
        data$mass <<- data$ratio <<- NULL
      }
      
      # peak table (FIXME: this could use some refactoring)      
      rawtable <- rawdata[subset(keys, value=="CPkDataListBox")$byteEnd:subset(keys, value=="CGCPeakList")$byteStart]
      arials <- grepRaw("([Arial][^\u0020-\u007e]){5}", rawtable, all=TRUE)
      #FIXME: newer versions of isodat (2.5 and 3.1 don't have this business, just 18 bytes between each label!)
      if (length(arials) < 5) {
        warning("peak table entries not found, this could be because the file might be created with a newer version (>2) of isodat. ",
                "files from isodat 2.5 and 3.1 are known to have this problem but are currently not yet supported")
      } else {
        entries<-NULL
        spos <- 9 + (regexpr("14000000fffeff08", paste(readBin(rawtable[1:(arials[1]-48)], "raw", n=(arials[1]-48)), collapse=""), fixed=TRUE)-1)/2
        for (i in arials) {
          epos<-(i-48)
          entries<-c(entries, paste(readBin(rawtable[spos:epos], "character", n=(epos-spos)/2, size=2), collapse=""))
          spos<-i+100
        }
        entries <- entries[-length(entries)] # last entry is garbage
        
        
        # sometimes there is an extra column (rps), sometimes not
        if (! (rps_column <- length(entries) %% 28 == 0) && # rps column
            ! (length(entries) %% 27 == 0)) { # no rps column
          # neither 27 nor 28 columns! not sure what's going on
          assign("isoread_debug", entries, env = globalenv())
          warning("it appears the peak table has neither exactly 27 nor 28 columns, not sure how to deal with this scenario. ",
                  "a dump of all entries recovered from the peak table is stored in the global variable 'isoread_debug'")
        } else {
          table<-matrix(entries, byrow=TRUE, ncol=if(rps_column) 28 else 27) # FIXME not sure this is always true that it's 27 columns but appears to be the case
          df<-data.frame(table[2:nrow(table),], stringsAsFactors=FALSE)
          names(df)<-table[1,]
          
          # add rps column if missing
          if (!rps_column)
            df$`Rps 3H2/2H2` <- ""
          
          # process peak nr
          if ( !('Peak Nr.' %in% names(df) ))
            stop("'Peak Nr.' column not found in peak table. Only available columns are ", paste0(names(df), collapse = ", "))
          
          peakNrPattern <- "^([0-9]+)([\\*\\+]?)$"
          df <- mutate(
            df,
            `Ref. Peak` = sub(peakNrPattern, '\\2', `Peak Nr.`) == "*", # whether it is a reference peak
            Status = sapply(sub(peakNrPattern, '\\2', `Peak Nr.`), # whether peak was added
                            function(x) { if (x=="+") "Added" else "Auto" }),
            Formula = "",
            `Peak Nr.` = as.integer(sub(peakNrPattern, '\\1', `Peak Nr.`))) # peak number as integer
          
          # store peak table data
          peakTable <<- df
        }
      } 
      
    },

    cleanup = function(clean_chrom_data = FALSE, ...) {
      # FIXME, this should ideally go into IrmsContinuousFlowDataClass but the method comes from BinaryFile$cleanup ...
      callSuper(...)
      if (clean_chrom_data)
        chromData <<- data.frame()
    },
    
    # COMPUTATION ================
    
    #' Evaluate data in peak table
    #' 
    #' This function uses the standards defined in the peak table to (re)evaluate the isotopic
    #' composition of the other peaks in this reference frame.
    #' 
    reevaluate_peak_table = function() {
      stop("not implemnted yet")
    },
    
    # VISUALIZATION / PLOTTING ==========================
    
    #' Make a ggplot of the references
    #'
    #' By default, this makes a plot that generates an overview
    #' of how close the reference peaks are to
    #' each other.
    plot_refs = function(
      y = to_delta(`rR 3H2/2H2`, mean(`rR 3H2/2H2`)), 
      ylab = "dD [permil] vs mean ratio", 
      title = "Variation in reference peaks"){
      # FIXME not sure how to call super from here to avoid this code replication
      do.call(.self$plot_peak_table, list(y = substitute(y), ylab = ylab, title = title, data = get_peak_table(type = "ref")))
    },
    
    #' Make a ggplot of the data
    #'
    #' By default, this makes a plot of the delta values.
    plot_data = function(
      y = `d 2H/1H`, 
      ylab = "dD [permil] vs VSMOW", 
      title = "Data peaks"){
      do.call(.self$plot_peak_table, list(y = substitute(y), ylab = ylab, title = title, data = get_peak_table(type = "data")))
    },
    
    get_info = function(show = c()) {
      info <- rbind(callSuper(), data.frame(Property = "Peaks in peak table", Value = as.character(nrow(peakTable))))
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
      print(head(chromData))
      cat("\nPeak Table:\n")
      print(peakTable)
    }
  )
)