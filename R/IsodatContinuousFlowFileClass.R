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
        amw_pos <- find_key("CAllMoleculeWeights", fixed = T)$byteStart - 1
        
        if (amw_pos <= pos)
          stop("End of data block appears to be before start of data block, something must be identified wrong here!", call. = FALSE)
        
        # extract gas configuration
        look_back_n <- 80 # how many bytes to look back
        end_of_data_pattern <- list(
          marker1 = "\xff\xfe\xff", # 3 long
          gap1 = "(\\x00|[\x01-\x04]){21}", # 21 long
          marker2 = "\xff\xfe\xff[\x01-\x04]", # 4 long
          gas = "([\x20-\x7e]\\x00){2,}", # gas name (at least 2 ascii long)
          marker3 = "\xff\xfe\xff",
          gap2 = "\\x00{5}\xff\xff\x02\\x00\x13\\x00",
          end = "$", # and then afterwards comes the CAllMoleculeWeightsblock
          bla = ""
        )
        
        eod_pos <- grepRaw(paste(end_of_data_pattern, collapse = ""), rawdata[(amw_pos - look_back_n):amw_pos]) - 1
        if (length(eod_pos) == 0) 
          stop("Could not find gas configuration in binary file, unexpectd data pattern before CAllMoleculeWeights block: ",
               get_raw_binary_data(rawdata[(end - 100):end]), call. = FALSE)
        
        # gas name
        data$Gas <<- get_unicode(rawdata[(amw_pos - look_back_n + eod_pos):amw_pos])
          
        # number of data points as control
        end <- amw_pos - look_back_n + eod_pos - 8 
        n <- (end - pos) / (4 + length(masses) * 8)

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
      
      # find first peak retention times (stupidly stored separately from the rest of the data)
      move_to_key("CGCPeakList")
       
      # retention time and amplitude block parsing (which are somewhat separate from table)
      parse_rt_amp_blocks <- function (.raw, label, offset = 1L) {
        # find either the CGCPeak tag or the tell tale hex sequence with 83 02 00 00
        re_block <- "((\x43\x47\x43\x50\x65\x61\x6b)|(\\x00\\x00[\x01-\xff]\x83\x02\\x00\\x00))"
        
        
        # return empty list if nothing found
        if (length(grepRaw(re_block, .raw, offset = offset)) == 0) return (list())
        
        # identify the repeat blocks (one for each mass, the retention times are repeated each time)
        # NOTE: the retention time is only ALMOST repeated each time, if there are significant 
        # chromatographic shifts (as is alwayst he case for H2), these will in fact NOT quite be
        # identical. Here we fetch the first one (assuming that tends to be the major ion peak) and
        # store it for all masses, but maybe it would be better to aquire all
        block_pos <- offset
        block_hex <- c()
        amp_vals <- c()
        rt_vals <- c()
        
        # loop until all found
        while (block_pos < length(.raw)) {
          block_pos <- grepRaw(re_block, .raw, offset = block_pos) 
          if (length(block_pos) == 0) break # done
          
          # offset
          start <- parse_binary_data(.raw[block_pos:(block_pos + 7)], "binary", length = 7)
          if (start == "43 47 43 50 65 61 6b") # = "CGCPeak"
            block_pos <- block_pos + 71L
          else
            block_pos <- block_pos + 68L
          
          # keep whole hex block for debugging
          block_hex <- c(block_hex, get_raw_binary_data(.raw[(block_pos):(block_pos + 44)]))
          
          # rts (only taking the one from one/first block assuming it's representative for all masses)
          if (length(rt_vals) == 0) {
            rt_vals <- c(
              `Start\n[s]` = parse_binary_data(.raw[(block_pos):(block_pos+7)], "double"),
              `Rt\n[s]` = parse_binary_data(.raw[(block_pos+20):(block_pos+27)], "double"),
              `End\n[s]` = parse_binary_data(.raw[(block_pos+40):(block_pos+47)], "double")
            )
          }
          
          # amplitudes
          amp_vals <- c(amp_vals, parse_binary_data(.raw[(block_pos + 28):(block_pos + 36)], "double"))
          
          # onward to next block
          block_pos <- block_pos + 44
        }
        
        # sanity check (none of the retention times should be negative or insanely large)
        if (any(rt_vals < 0) || any(rt_vals < 1e-100) || any(rt_vals > 1e100)) {
          stop("There was a problem trying to automatically parse the results table.\n",
               "Retention time values do not make sense (cell label '", label, "'):\n",
               paste(paste0(sub("\\n", " ", names(rt_vals)), " = ", rt_vals), collapse = "\n"), 
               "\n In block:\n", block_hex[1],
               call. = F)
        }
        
        # sanity check (need as many amplitude values as masses)
        if (length(masses) != length(amp_vals)) {
          stop("There was a problem trying to automatically parse the results table.\n",
               "Did not find the right number of amplitude values for peak at retention time ", round(rt_vals[2], 3), 
               " for all masses ", paste(masses, collapse = ", "),
               ": amplitude  (sequences in parenthesis):\n",
               paste(paste0(round(amp_vals, 2), " (", block_hex, ")"), collapse = "\n"),
               call. = F)
        }
        names(amp_vals) <- paste0("Ampl ", sub("mass", "", masses), "\n[mV]")
        
        return(as.list(c(rt_vals, amp_vals)))
      }
      
      # row data
      row_data <- list()
      row <- 1
      
      # parse the first retention time and amplitude block (which does not fit the rest of the cell pattern)
      row_data$r1 <- 
        parse_rt_amp_blocks(
          rawdata[(pos+16):find_key("CEvalDataItemTransferPart", occ = 1, fix = T)$byteStart],
          label = "CGCPeak")
      
      # all the data values
      rawtable <- rawdata[find_key("CEvalDataIntTransferPart", occ = 1, fix = T)$byteEnd:
                            find_key("DetectorDataBlock", occ = 1, fix = T)$byteStart]
      # table cell pattern
      re_pattern <- list(
        label = "([\x20-\x7e]\\x00){2,}", # label (at least 5 ascii long)
        gap1 = "\xff\xfe\xff[^\xff]+\xff\xfe\xff.([\x20-\x7e]\\x00)*\xff\xfe\xff\\x00\xff\xfe\xff.", # gap
        units1 = "([\x20-\x7e]\\x00){1,}", # units (at least 1 ascii long)
        gap2 = "\xff\xfe\xff.", # gap between units
        units2 = "([\x20-\x7e]\\x00){1,}", # second part of units (vs.)
        gap3 = ".\\x00{3}\xff\xfe\xff.\\x00*([^\\x00]\\x00{4,})+", # gap before value
        mode = "[\x01-\x08]\\x00\\x00\\x00", # mode block
        type = "[\x01-\x08]\\x00\\x00\\x00" # type block
      )
      
      # find all cell dividers using the combined pattern
      dividers <- c(
        grepRaw(paste(re_pattern, collapse = ""), rawtable, all=TRUE), 
        length(rawtable)) 
      new_data <- list()
      
      # loop through dividers
      for (i in 2:length(dividers)) {
        
        row_id <- paste0("r", row)
        if (is.null(row_data[[row_id]])) row_data[[row_id]] <- list()
        
        rawcell <- rawtable[dividers[i-1]:(dividers[i]-1)]
        
        # recover each segment
        raw_segs <- list()
        position <- 1L
        for (seg in names(re_pattern)) {
          raw_segs[[seg]] <- grepRaw(re_pattern[[seg]], rawcell, value = TRUE, offset = position)
          position <- position + length(raw_segs[[seg]])
        }
        hex_segs <- lapply(raw_segs, function(i) parse_binary_data(i, "binary", length = length(i)))
        start_of_value <- sum(sapply(raw_segs, length)) + 1
        
        # label check
        label <- rawToChar(raw_segs$label[c(TRUE, FALSE)])
        if (label %in% c("Overwritten"))
          next # we're not reading this tag
      
        
        # general check
        if (any(sapply(raw_segs, length) == 0 )) {
          stop("There was a problem trying to automatically parse the results table.\n",
                  "Can't find all table cell segments for label '", label, "' within:\n",
                  parse_binary_data(rawcell, "binary", length = length(rawcell)),
                  "\n", paste(paste0(names(hex_segs), ": ", hex_segs), collapse = "\n"),
          call. = FALSE)
        }
        
        # data type and check
        value_mode <- parse_binary_data(raw_segs$mode, "binary")
        value_type <- parse_binary_data(raw_segs$type, "binary")
        type <- 
          if (value_mode == "01") "character"
        else if (value_mode == "02" && value_type %in% c("08", "03")) "numeric"
        else if (value_mode == "02" && value_type == "04") "integer"
        else if (value_mode == "02" && value_type == "01") NA_character_
        else  {
          stop("There was a problem trying to automatically parse the results table.\n",
               "Can't figure out what to do with the combination of value mode (", value_mode, ") and type (",
               value_type, ") for label '", label, "' within:\n",
               parse_binary_data(rawcell, "binary", length = length(rawcell)),
               "\n", paste(paste0(names(values), ": ", values), collapse = "\n")
               )
        }
        
        # cell value
        if (!is.na(type)) {
          value <- 
            if (type == "character") 
              rawToChar(grepRaw("^([\x20-\x7e]\\x00){1,}", 
                                rawcell[start_of_value:length(rawcell)], value = TRUE)[c(TRUE, FALSE)])
          else if (type == "numeric") 
            parse_binary_data(rawcell[(start_of_value):(start_of_value+8)], "double")
          else if (type == "integer") 
            as.integer(parse_binary_data(rawcell[(start_of_value):(start_of_value+4)], "short"))
          else stop ("should never get here!")
          
          key <- paste0(label, "\n")
          if (! (unit <- rawToChar(raw_segs$units1[c(TRUE, FALSE)])) %in% c("", " "))
            key <- paste0(key, unit)
          if (! (vs <- rawToChar(raw_segs$units2[-c(1:4)][c(TRUE, FALSE)])) %in% c("", " ")) 
            key <- paste0(key, " vs ", vs)
          
          # add new data
          new_data <- c(new_data, setNames(list(value), key))
        }
        
        # store new values in table
        for (key in names(new_data)) {
          if (!is.null(row_data[[row_id]][[key]]) && row_data[[row_id]][[key]] != new_data[[key]]) {
            stop("There was a problem trying to automatically parse the results table.\n",
                 "Can't figure out what to do with apparent non-identical replicate column entries in the same row:\n",
                 "Found ", sub("\\n", "", key), " values ", row_data[[row_id]][[key]], " and ", new_data[[key]], "\nLatter block:",
                 parse_binary_data(rawcell, "binary", length = length(rawcell)), call. = FALSE)
          }
          row_data[[row_id]][[key]] <- new_data[[key]]
        }
        
        # look for end of row sequence
        re_row_end <- "(\xff\xfe\xff.([\x20-\x7e]\\x00)+){3}\xff\xfe\xff.{117}" 
        if (length(grepRaw(re_row_end, rawcell, offset = start_of_value)) > 0) {
          row <- row + 1
        }
        
        # retention time and amplitude (belongs to the next set so processed after row++)
        new_data <- parse_rt_amp_blocks(rawcell, label, offset = start_of_value)
        
      } # END OF table cell loop
      
      # convert to data table
      dataTable <<- as.data.frame(dplyr::bind_rows(row_data))
      
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