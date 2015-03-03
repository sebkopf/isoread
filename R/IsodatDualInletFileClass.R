#' @include IsodatFileClass.R
#' @include IrmsDualInletDataClass.R
NULL

#' Clumped dual inlet data class
#' 
#' 
#' @name IsodatDualInletFile
#' @exportClass IsodatDualInletFile
#' @seealso \link{BinaryFile}, \link{IsodatFile}, \link{IrmsDualInletData}, \link{IrmsData}
IsodatDualInletFile <- setRefClass(
  "IsodatDualInletFile",
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
      # overwrite in derived classes and set data table definitions properly!
      # see IrmsDualInletDataClass for details on requirements and functionality
    },
    
    # READ DATA =========================
    
    #' expand process function specifically for dual inlet type data
    process = function(...) {
      callSuper()
      
      # find recorded masses
      masses <- find_key("Mass \\d+",
          byte_min = find_key("CTraceInfo", occ = 1, fix = T)$byteEnd,
          byte_max = find_key("CPlotRange", occ = 1, fix = T)$byteStart)$value
      
      if (length(masses) == 0)
        stop("Error: no keys named 'Mass ..' found. Cannot identify recorded mass traces in this file.")
      
      # unless mass plot options are already manually defined (in init_irms_data), define them automatically here and assign colors
      mass_names <- sub("Mass (\\d+)", "mass\\1", masses)
      if (length(plotOptions$masses) == 0) {
        # color blind friendly pallete (9 colors)
        palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#D55E00", "#0072B2", "#CC79A7", "#999999", "#F0E442")
        if (length(masses) > length(palette))
          stop("Currently only supporting up to ", length(palette), " automatically assigned different colors for masses but ",
              "this file is recording data for ", length(masses), " different masses. Plesae define the plotOptions manually.")        
        
        set_plot_options(
          masses = setNames(
            sapply(seq_along(masses), function(i) list(list(label = masses[i], color = palette[i]))), 
            mass_names)
        )
      }
      
      # extract raw voltage data from the cycles
      raw_data_keys <- find_key("^(Standard|Sample) \\w+$",
          byte_min = find_key("CDualInletRawData", occ = 1, fix = T)$byteEnd, 
          byte_max = find_key("CTwoDoublesArrayData", occ = 1, fix = T)$byteStart)
      
      if (nrow(raw_data_keys) == 0)
        stop("could not find raw data in this file")
  
      # extract cycle information
      raw_data_keys <- mutate(raw_data_keys,
                              analysis = sub("^(Standard|Sample) (\\w+)$", "\\1", value),
                              cycle.0idx = sub("^(Standard|Sample) (\\w+)$", "\\2", value), # 0 based index, adjust in next line
                              cycle = ifelse(cycle.0idx == "Pre", 0, suppressWarnings(as.integer(cycle.0idx)) + 1L))
      n_cycles <- max(raw_data_keys$cycle)
      
      # read in all masses and cycles
      massData <<- do.call(data.frame, 
                          args = c(list(stringsAsFactors = FALSE, analysis = character(), cycle = integer()), 
                                   lapply(plotOptions$masses, function(i) numeric())))
      
      for (i in 1:nrow(raw_data_keys)) {
          move_to_key(raw_data_keys[i, ])
          has_intensity_block <- nrow(subset(keys, value == "CIntensityData" & byteStart > raw_data_keys[i, "byteStart"] & byteEnd < raw_data_keys[i, "byteEnd"] + 64)) > 0
          massData[i, ] <<- c(list(raw_data_keys[i, "analysis"], raw_data_keys[i, "cycle"]), 
            as.list(parse("double", length = length(mass_names), skip_first = if (has_intensity_block) 82 else 64)))
      }
      
      # evaluated data / data table
      # NOTE: this could (should ?) be calculated from the raw voltage data directly
      eval_data_keys <- find_key("^(d |AT).+$",
                                byte_min = find_key("CDualInletEvaluatedData", occ = 1, fix = T)$byteEnd, 
                                byte_max = find_key("Gas Indices", occ = 1, fix = T)$byteStart)
      if (nrow(eval_data_keys) == 0)
        stop("could not find evaluated data in this file")
      
      eval_data <- list(cycle = 1:n_cycles)
      for (i in 1:nrow(eval_data_keys)) {
        move_to_key(eval_data_keys[i,])
        gap_to_data <- switch(
          substr(eval_data_keys[i, "value"], 1, 2), 
          `d ` = 54, `AT` = 50) 
        # these are evaluated data points for ALL cycles 
        # (each data point starts with 8 x00 characters, so technically the gap is 62/58 instead of 54/50)
        # Note: the keys all have a space at the end --> trailing white spaces removed
        eval_data[[sub("\\s+$", "", eval_data_keys[i,"value"])]] <- 
          parse("double", length = 2 * n_cycles, skip_first = gap_to_data)[c(FALSE, TRUE)] 
      }
      dataTable <<- data.frame(eval_data, check.names = F)
      
      # unless dataTableColumns are already manually defined, define them here
      if (nrow(dataTableColumns) == 0) {
        dataTableColumns <<- 
          data.frame(data = names(dataTable), column = names(dataTable), 
                     units = "", type = "numeric", show = TRUE, stringsAsFactors = FALSE)
      }
      
      # grid infos
      extract_measurement_info()
      
      # sequence line information
      extract_sequence_line_info()
      
    },
    
    # EXPORT ===================
    
    #' export data (by default to csv)
    export_data = function(file = default_filename(), type = c("raw", "table", "summary", "info"), extension = "csv", sep = ",", headers = TRUE, ...) {
      # what to export
      if (missing(type)) type <- "summary"
      type <- match.arg(type)
      
      # default filename
      default_filename <- function() {
        file.path(.self$filepath, paste0("export_", .self$filename, "_", type, ".", extension))
      }
      
      message("Creating ", type, " export for ", .self$filename, " ...")
      if (type == "raw") 
        write.table(get_mass_data(), file, sep = sep, row.names = FALSE, col.names = headers, ...)
      else if (type == "table")
        write.table(get_data_table(), file, sep = sep, row.names = FALSE, col.names = headers, ...)
      else if (type == "summary")
        write.table(get_data_table(summarize = TRUE), file, sep = sep, row.names = FALSE, col.names = headers, ...)
      else if (type == "info")
        write.table(get_info(), file, sep = sep, row.names = FALSE, col.names = headers, ...)
      message(type, " data exported to ", file)
    },
    
    #' custom show function to display roughly what data we've got going
    show = function() {
      cat("\nShowing summary of", class(.self), "\n")
      callSuper()
      cat("\n\nMass data:\n")
      print(get_mass_data())
      cat("\n\nData table:\n")
      print(get_data_table(summarize = TRUE))
    }
  )
)