#' @include BinaryFileClass.R
NULL

#' Isodat file class
#' 
#' Class representing an isodat binary file.
#' 
#' @name IsodatFile
#' @seealso \link{BinaryFile}
IsodatFile <- setRefClass(
  "IsodatFile",
  contains = "BinaryFile",
  fields = list (),
  methods = list(
    
    extract_measurement_info = function() {
      "extracts the measurement information"
      
      rawtable <- rawdata[subset(keys, value=="CMeasurmentInfos")$byteEnd:subset(keys, value=="CMeasurmentErrors")$byteStart]
      dividers <- c(grepRaw("\xff\xfe\xff", rawtable, all=TRUE), length(rawtable))
      if (length(dividers) == 0) 
        stop("this file does not seem to have the expected hex code sequence FF FE FF as dividers in the grid info")
      
      for (i in 2:length(dividers)) {
        # read ASCII data for each block
        raw_ascii <- grepRaw("([\u0020-\u007e][^\u0020-\u007e])+", rawtable[(dividers[i-1]+4):dividers[i]], all=T, value = T)
        x <- if (length(raw_ascii) > 0) rawToChar(raw_ascii[[1]][c(TRUE, FALSE)]) else ""
        if (x == "CUserInfo") {
          id <- paste0("Info_", sub("^(\\w+).*$", "\\1", value))
          if (!is.null(data[[id]])) 
            data[[id]] <<- c(data[[id]], value) # append value with first word as ID
          else
            data[[id]] <<- value # store new value with first word as ID
        }
        else value <- x # keep value
      }
    },
    
    extract_sequence_line_info = function() {
      "extracts the sequence line information"
      
      # find sequence line information
      rawtable <- rawdata[subset(keys, value=="Sequence Line Information")$byteEnd:subset(keys, value=="Visualisation Informations")$byteStart]
      if (length(rawtable) < 10)
        stop("this file does not seem to have a data block for the sequence line information")
      
      dividers <- grepRaw("\xff\xfe\xff", rawtable, all=TRUE)
      if (length(dividers) == 0) 
        stop("this file does not seem to have the expected hex code sequence FF FE FF as dividers in the sequence line information")
      
      for (i in 2:length(dividers)) {
        # read ASCII data for each block
        raw_ascii <- grepRaw("([\u0020-\u007e][^\u0020-\u007e])+", rawtable[(dividers[i-1]+4):dividers[i]], all=T, value = T)
        x <- if (length(raw_ascii) > 0) rawToChar(raw_ascii[[1]][c(TRUE, FALSE)]) else ""
        if (i %% 2 == 1) data[[x]] <<- value # store key / value pair in data list
        else value <- x # keep value for key (which comes AFTER its value)
      }
      
    }
    
    )
)