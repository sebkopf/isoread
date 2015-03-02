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
    
    #' @param read_mass_data whether to read chromatographic mass data (can be a lot of data)
    process = function(read_mass_data = TRUE, ...) {
      
      # find all recorded masses
      masses <- sub("^Intensity (\\d+)$", "mass\\1", unique(find_key("^Intensity \\d+$")$value))
      
      if (length(masses) == 0)
        stop("Error: no keys named 'Intensity ..' found. Cannot identify recorded mass traces in this file.")
      
      # unless mass plot options are already manually defined (in init_irms_data), define them automatically here and assign colors
      if (length(plotOptions$masses) == 0) {
        # color blind friendly pallete (9 colors)
        palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#999999", "#F0E442")
        if (length(masses) > length(palette))
          stop("Currently only supporting up to ", length(palette), " automatically assigned different colors for masses but ",
               "this file is recording data for ", length(masses), " different masses. Plesae define the plotOptions manually.")        
        
        set_plot_options(
          masses = setNames(
            sapply(seq_along(masses), function(i) list(list(label = masses[i], color = palette[i], offset = 0))), 
            masses)
        )
      }
      
      # extract raw voltage data from the continuous flow recording
      if (read_mass_data) {
        move_to_key("CEvalGCData")
        skip(8) # not sure what's in here
        
        # find end of the data block
        end <- find_key("CAllMoleculeWeights", fixed = T)$byteStart
        
        if (end <= pos)
          stop("End of data block appears to be before start of data block, something must be identified wrong here!")
        
        # number of data points (there appears to be a 56 byte gap at the end, not sure what's in there)
        n <- (end - pos - 56) / (4 + length(masses) * 8)
        
        if (n %% 1 > 0) {
          # there was a problem here, proceed with caution (and floored n value)
          warning("Something doesn't quite add up in this file, the number of data points was identified as ", 
                  round(n, 3), " which is not a whole integer.\nPlease proceed with caution.")
          n <- floor(n)
        }
        
        # parse intensity data
        parse_array(
          types = c(time = "float", sapply(masses, function(i) list("double"))), 
          n = n, id = "mass")
        
        massData <<- data$mass
        data$mass <<- NULL
      }
      
      # measurement info
      extract_measurement_info()
      
      # sequence line info
      extract_sequence_line_info()
      
      ##### data table #####
      out <- list()
      
      # find first peak retention times (stupidly stored separately from the rest of the data)
      move_to_key("CGCPeakList")
      # this is the "CGCPeak" key (too short to make it into the keys)
      skip(grepRaw("\x43\x47\x43\x50\x65\x61\x6B", rawdata, offset = pos) + 7 + 64 - pos)
      start_pos <- pos
      start_binary <-  parse("binary", length = 8)
      out$`Start\n[s]` <- parse("double", skip_first = -8)
      #bg1 <- parse("double") # I think this is what's here, not currently used though
      out$`Rt\n[s]` <- parse("double", skip_first = 12) 
      out$`End\n[s]` <- parse("double", skip_first = 12)
      
      # find the amplitudes from where the start retention times are
      for (mass in sub("mass", "", masses)) {
        pos <<- as.integer(grepRaw(gsub(" ", "\\\\x", paste0(" ", start_binary)), rawdata, offset = start_pos) + 28)
        out[[paste0("Ampl ", mass, "\n[mV]")]] <- parse("double")
        start_pos <- pos
      }
      
      # all the data values
      rawtable <- rawdata[find_key("CEvalDataIntTransferPart", occ = 1, fix = T)$byteEnd:
                            find_key("DetectorDataBlock", occ = 1, fix = T)$byteStart]
      
      # data value pattern
      dividers <- 
        c(grepRaw(
          paste0("([\x20-\x7e]\\x00){2,}", # label (at least 5 ascii long)
                 "\xff\xfe\xff[^\xff]+\xff\xfe\xff.([\x20-\x7e]\\x00)*\xff\xfe\xff\\x00\xff\xfe\xff.", # gap
                 "([\x20-\x7e]\\x00){1,}", # units (at least 1 ascii long)
                 "\xff\xfe\xff.", # gap
                 "([\x20-\x7e]\\x00){1,}", # second part of units (vs.)
                 ".\\x00{3}\xff\xfe\xff.", # spacer after units
                 "\\x00*[^\\x00]\\x00{3}[^\\x00]\\x00{3}" # gap before value
          ),
          rawtable, all=TRUE), length(rawtable))
      
      # loop through dividers
      for (i in 2:length(dividers)) {
        rawcell <- rawtable[dividers[i-1]:(dividers[i]-1)]
        
        # label and units
        label <- grepRaw("^([\x20-\x7e]\\x00){2,}", rawcell, value = TRUE)
        end_of_gap <- grepRaw("\xff\xfe\xff\\x00\xff\xfe\xff.([\x20-\x7e]\\x00){1,}", rawcell) + 8
        units <- grepRaw("([\x20-\x7e]\\x00){1,}", rawcell, value = TRUE, offset = end_of_gap - 1)
        vs <- grepRaw("\xff\xfe\xff.([\x20-\x7e]\\x00){1,}", rawcell, value = TRUE, offset = end_of_gap + length(units) - 1)
        
        # value data type
        end_of_spacer <- end_of_gap + length(units) + length(vs) + 8 # position after vs and spacer
        start_of_value <- 
          grepRaw("[\x01-\x08]\\x00{3}[\x01-\x08]\\x00{3}", rawcell, 
                  offset = end_of_spacer) + 8
        value_mode <- parse_binary_data(rawcell[start_of_value-8], "binary")
        value_type <- parse_binary_data(rawcell[start_of_value-4], "binary")
        type <- 
          if (value_mode == "01") "character"
        else if (value_mode == "02" && value_type == "08") "numeric"
        else if (value_mode == "02" && value_type == "04") "integer"
        else if (value_mode == "02" && value_type == "01") NA_character_
        else stop("There was a problem trying to automatically parse the results table.\n",
                  "Can't figure out what to do with this combination of value mode and type: ", 
                  value_mode, " / ", value_type)
        
        # look for retention time block
        pattern <- "(\xff\xfe\xff.([\x20-\x7e]\\x00)+){3}\xff\xfe\xff.{117}" # gap from last value to RT
        rt_start <- 
          grepRaw(pattern, rawcell, offset = start_of_value + 8) +
          length(grepRaw(pattern, rawcell, offset = start_of_value + 8, value = TRUE))
        if (length(rt_start) > 0) {
          out[["Start\n[s]"]] <- c(out[["Start\n[s]"]], 
                                   parse_binary_data(rawcell[(rt_start):(rt_start+8)], "double"))  
          out[["Rt\n[s]"]] <- c(out[["Rt\n[s]"]],
                                parse_binary_data(rawcell[(rt_start+20):(rt_start+28)], "double") )  
          out[["End\n[s]"]] <- c(out[["End\n[s]"]], 
                                 parse_binary_data(rawcell[(rt_start+40):(rt_start+48)], "double"))  
          
          start_binary <-  parse_binary_data(rawcell[(rt_start):(rt_start+8)], "binary", length = 8)
          
          # find the amplitudes from where the start retention times are
          value_pos <- 1L
          for (mass in sub("mass", "", masses)) {
            value_pos <- grepRaw(gsub(" ", "\\\\x", paste0(" ", start_binary)), rawcell, offset = value_pos) + 28
            out[[paste0("Ampl ", mass, "\n[mV]")]] <- 
              c(out[[paste0("Ampl ", mass, "\n[mV]")]],
                parse_binary_data(rawcell[(value_pos):(value_pos+48)], "double"))
          } 
        }
      
        # pull out value and store in list
        if (!is.na(type)) {
          value <- 
            if (type == "character") 
              rawToChar(grepRaw("^([\x20-\x7e]\\x00){1,}", rawcell[start_of_value:length(rawcell)], value = TRUE)[c(TRUE, FALSE)])
          else if (type == "numeric") parse_binary_data(rawcell[(start_of_value):(start_of_value+8)], "double")
          else if (type == "integer") as.integer(parse_binary_data(rawcell[(start_of_value):(start_of_value+4)], "short"))
          else stop ("should never get here!")
          key <- paste0(rawToChar(label[c(TRUE, FALSE)]), "\n")
          if (! (unit <- rawToChar(units[c(TRUE, FALSE)])) %in% c("", " "))
            key <- paste0(key, unit)
          if (! (vs <- rawToChar(vs[-c(1:4)][c(TRUE, FALSE)])) %in% c("", " ")) 
            key <- paste0(key, " vs ", vs)
          out[[key]] <- c(out[[key]], value)      
        }
      }
      
      # convert to data table
      columns <- subset(ldply(out, function(i) data.frame(n = length(i)), .id = "key"), n > 1)
      
      if (!all(columns$n == columns$n[1]))
        stop("Error: not all data table columns seem to have the same number of rows: ", 
             paste(paste0(sub("\n", "", columns$key), " (", columns$n, ")"), collapse = ", "))
      dataTable <<- data.frame(out[columns$key], check.names = F, stringsAsFactors = F)
      
      # unless dataTableColumns are already manually defined, define them here from the dataTable
      if (nrow(dataTableColumns) == 0) {
        # NOTE: the atual table columns that are relevant are listed in the file at the beginnig
        # FIXME: incorporate only the ones listed there, currently just taking them all
        dataTableColumns <<- mutate(data.frame(
          data = names(dataTable),
          type = vapply(dataTable, class, USE.NAMES=F, FUN.VALUE = "character"),
          show = TRUE,
          stringsAsFactors = FALSE),
          column = sub('^([^\n]+).*', '\\1', data), # pull out column names from the data names
          units = sub('[^\n]+\n([^\n]*)$', '\\1', data)) # pull out units from the data
        
        # if name column is missing --> add it with empty value
        if (! dataTableKeys[['name']] %in% dataTableColumns$column) {
          dataTableColumns <<- rbind(
            mutate(dataTableColumns[1,],
                   data = dataTableKeys[['name']], type = "character",
                   column = dataTableKeys[['name']], units = ""),
            dataTableColumns)
          dataTable[[dataTableKeys[['name']]]] <<- ""
        }
        
        if ( length(missing <- setdiff(dataTableKeys, dataTableColumns$column)) > 0)
          stop("Some key ID columns in the data table are missing: ", 
               paste(missing, collapse = ", "))
      }
    
    },
    
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