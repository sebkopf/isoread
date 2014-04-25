#' Convenience wrapper for parsing binary data.
#' For more details on reading binary data, check ?readBin
#' 
#' @param type data type see \code{\link{map_binary_data_type}} for details
#' @param length how many instances of this object (for characters and raw this means length of string, all others a vector)
#' @return read data
#' @export
parse_binary_data = function(data, type, length = 1) {
  # map data type
  mapped_type <- map_binary_data_type(type)
  
  # different reads
  if (type == "binary")
    read <- paste(readBin(data, "raw", n = length, size = 1), collapse=" ")
  else if (type == "UTF8")
    read <- rawToChar(readBin(data, "raw", n = length, size=1))
  else if (mapped_type$class == "character")
    read <- paste(readBin(data, "character", n = length, size = mapped_type$nbyte), collapse="")
  else
    read <- readBin(data, mapped_type$class, n = length, size = mapped_type$nbyte)
  
  return(read)
}

#' Maps binary C data types to proper R data types and byte lengths
#' @param type
#' \itemize{
#'    \item{'binary'}{ = raw with 1 byte (raw data)}
#'    \item{'UTF8'}{ = character with 1 byte (ascii)}
#'    \item{'UTF16'}{ = character with 2 bytes (unicode)}
#'    \item{'UTF32'}{ = character with 4 bytes (unicode)}
#'    \item{'short'}{ = integer with 2 bytes (16bit)}
#'    \item{'long'}{ = integer with 4 bytes (32bit)}
#'    \item{'longlong'}{ = integer with 8 bytes (64bit)}
#'    \item{'float'}{ = numeric with 4 bytes (32bit)}
#'    \item{'double'}{ = numeric with 8 bytes (64bit)}
#' }
#' @note implemented signed int and complex if needed
#' @export
map_binary_data_type <- function(
  type = c('binary', 'UTF8', 'UTF16', 'UTF32', 'short', 
           'long', 'long long', 'float', 'double')) {
  switch(
    type,
    binary = list(class = 'raw', nbyte = 1),
    UTF8 = list(class = 'character', nbyte = 1),
    UTF16 = list(class = 'character', nbyte = 2),
    UTF32 = list(class = 'character', nbyte = 4),
    short = list(class = 'integer', nbyte = 2),
    long = list(class = 'integer', nbyte = 4),
    longlong = list(class = 'integer', nbyte = 8),
    float = list(class = 'numeric', nbyte = 4),
    double = list(class = 'numeric', nbyte = 8),
    stop("not a valid data type: '", type, "'"))
}


#' Binary File reference class
#' @rdname BinaryFile
#' @exportClass BinaryFile
BinaryFile <- setRefClass(
  "BinaryFile",
  fields = list (
      filepath = 'character',
      filename = 'character',
      rawdata = 'raw',
      keys = 'data.frame',
      data = 'list',
      pos = 'integer' # current position in data stream
    ),
  methods = list(
    initialize = function(file, ...){
      if (!missing(file))
        initFields(
          filename = basename(file),
          filepath = dirname(file))
      callSuper(...)
    },
    
    #' load the data from the file and generate key lookup
    #' @exportMethod
    load = function(...) {
      read_file()
      find_keys()
    },
    
    #' process  the raw data to generate to fill the data list
    #' @exportMethod
    #' @note override
    process = function(...) {
      if (length(rawdata) == 0)
        stop("no data available, make sure load() was called and the binary file ",
             "is properly loaded (for example, have a look at the keys field)")
    },
    
    #' parse binary data at current position in the data stream
    #' advances pointer by the size of the read data 
    #' 
    #' @param type see \code{\link{map_binary_data_type}}
    #' @param length see \code{\link{parse_binary_data}}
    #' @param id if provided, will store the parsed data with this key in the \code{$data} field
    #' @param skip_first how many bytes to skip before reading this
    parse = function(type, length = 1, id = NA, skip_first = 0) {
      # skip
      skip(skip_first)
      
      # data type
      mapped_type <- map_binary_data_type(type)
      
      # read data (pass along a chunk of the rawdata that has the maximal possible read length)
      read <- parse_binary_data(rawdata[pos:(pos + mapped_type$nbyte * length)], type = type, length = length) 
      
      # advance position
      skip(mapped_type$nbyte * length) 
      
      # store
      if (!is.na(id))
        data[[id]] <<- read
     
      return(read)
    },
    
    #' repeatedly read the same set of information into a data frame
    #' @param types a named vector of data types (for data types see \code{\link{parse_binary_data}), 
    #' the names are used for the columns of the resulting data frame
    #' @param id if provided, will store the parsed data with this key in the $data field
    #' @param n length of array
    #' @param skip_first how many bytes to skip before reading this
    parse_array = function(types, n, id = NA, skip_first = 0) {
      # skip
      skip(skip_first)
      
      # get data types, nbytes and subset of rawdata for the array
      types <- as.pairlist(unlist(types))
      nbytes <- sapply(types, function(i) map_binary_data_type(i)$nbyte)
      subdata <- rawdata[pos:(pos + sum(nbytes) * n - 1)]
      df <- data.frame(read = 1:n, stringsAsFactors = F)
      
      # parse array
      for (i in seq_i <- seq_along(types)) {
        # vector of T/F for each byte whether it belongs to this column
        bytes_select <- unlist(sapply(seq_i, function(k) rep(i == k, nbytes[k]))) 
        df[names(types)[i]] <- parse_binary_data(
          subdata[rep(bytes_select, times=n)], type = types[[i]], length = n) 
      }
      
      # advance position
      skip(sum(nbytes) * n)
      
      # store
      if (!is.na(id))
        data[[id]] <<- df
      
      return (df)
    },
    
    #' skip number of bytes
    skip = function(nbyte) {
      pos <<- as.integer(pos + nbyte)
    },
    
    #' find a key by a regexp pattern
    find_key = function(pattern, occurence = 1) {
      if (nrow(keys) == 0)
        stop("no keys available, make sure load() was called")
      
      if (nrow(match <- keys[grep(pattern, keys$value),]) == 0)
        stop("pattern '", pattern, "' was not found")
      
      if (occurence == -1) occurence <- nrow(match)
      
      if (occurence > nrow(match))
        stop("pattern '", pattern, "' was found but only has ", nrow(match), " occurences ",
             "(trying to select occurence #", occurence, ")")
      
      return(match[occurence, "value"])
    },
    
    #' moves position to the end of a specific key
    #' @param key name of the key
    #' @param occurence which one to move to (first, second, third found?), use -1 for last
    move_to_key = function(key, occurence = 1) {
      if (nrow(keys) == 0)
        stop("no keys available, make sure load() was called")
      
      if (nrow(match <- subset(keys, value==key)) == 0)
        stop("key '", key, "' was not found")
      
      if (occurence == -1) occurence <- nrow(match)
      
      if (occurence > nrow(match))
        stop("key '", key, "' was found but only has ", nrow(match), " occurences ",
             "(trying to select occurence #", occurence, ")")
      
      pos <<- as.integer(match[occurence, "byteEnd"]) + 1L
    },
    
    #' read the binary file
    #' @note this does not work for large, files probably because of the 2^31-1 
    #' limit on vector size! think about ways to fix this...
    #' --> might have to acually read directly from the conection instead of the raw data buffer!
    read_file = function(){
      path <- file.path(filepath, filename)
      if (is.na(size <- file.info(path)$size))
        stop("file does not exist: ", path)
      
      # read
      con <- file(path, "rb")
      rawdata <<- readBin(con, raw(), n = size)
      close(con)
    },
    
    #' finds all unicode and ascii strings and stores them for navigation around the file
    find_keys = function(asciiL = 10, unicodeL = 5) {
      ascii <- find_ascii(asciiL)
      unicode <- find_unicode(unicodeL)
      keys <<- rbind(ascii, unicode)
      keys <<- keys[order(keys$byteStart),] 
      keys$byteGap <<- 
        diff(sort(c(keys$byteStart, keys$byteEnd, length(rawdata))))[c(FALSE,TRUE)] # add byte gap
    },
    
    #' find all ascii strings in the raw data
    #' @param minlength minimum length of continuous ascii characters
    #' @return data frame of found ascii strings
    #' FIXME: it appears that after each string, there are 3x null character (i.e. 00 00 00) --> use this to make finding strings better! (couldn't quite figure out how to recognize 00 characters)
    find_ascii = function(minlength) {
      regexp<-paste("[\u0020-\u007e]{", minlength, ",}", sep="")
      text <- data.frame(
        byteStart = grepRaw(regexp, rawdata, all=TRUE), #get ANSII strings
        value = ldply(grepRaw(regexp, rawdata, all=TRUE, value=TRUE), 
                      function(x) rawToChar(x))$V1, encoding='ASCII', stringsAsFactors=FALSE)
      text$byteEnd<-text$byteStart + nchar(text$value) - 1
      text$byteLength<-text$byteEnd - text$byteStart + 1
      text$strLength<-text$byteLength
      return(text)
    },
    
    #' find all unicode strings in the raw data
    #' @param minlength minimun length of unicode characters (each are 2 bytes long)
    #' @return data frame with found unicode strings
    find_unicode = function(minlength) {
      regexp<-paste("([\u0020-\u007e][^\u0020-\u007e]){", minlength, ",}", sep="")
      text<-data.frame(
        byteStart = grepRaw(regexp, rawdata, all=TRUE), #get Unicode strings
        value = ldply(grepRaw(regexp, rawdata, all=TRUE, value=TRUE), 
                      function(x) rawToChar(x[c(TRUE, FALSE)]))$V1,
        #paste(readBin(x, "character", n=length(x)/2, size=2), collapse=""))$V1, 
        encoding='Unicode', stringsAsFactors=FALSE)
      text$byteEnd<-text$byteStart + nchar(text$value) * 2 - 1
      text$byteLength<-text$byteEnd - text$byteStart + 1
      text$strLength<-text$byteLength/2
      return(text)
    },
    
    #' clean up text by removing randomly found strings that are clearly not proper targets
    #' @return vector of removed keys
    clean_keys = function(removeText = NULL, removePattern = NULL, unlessByteLength = 0, unlessText = NULL) {
      rem_text <- rem_pattern <- except_length <- except_text <- integer()
      
      if (!is.null(removeText))
        rem_text <- which(keys$value %in% removeText)
      
      if (!is.null(removePattern))
        rem_pattern <- grep(removePattern, keys$value)
      
      if (unlessByteLength > 0)
        except_length <- which(keys$byteLength >= unlessByteLength)
      
      if (!is.null(unlessText))
        except_text <- which(keys$value %in% unlessText)
      
      # figure out which ones to remove
      rem <- union(rem_text, rem_pattern)
      except <- union(except_length, except_text)
      rem <- setdiff(rem, except)
      
      # remove keys
      rem_keys <- keys[rem, "value"]
      if (length(rem) > 0) {
        keys <<- keys[-rem,]      
        # re calculate gaps between occuring strings
        keys$byteGap <<- diff(sort(c(keys$byteStart, keys$byteEnd, max(keys$byteEnd))))[c(FALSE,TRUE)] # add byte gap
      }
      return(rem_keys)
    },
    
    #' Show some info about this binary file class
    show = function() {
      cat("\nBinary File information:")
      cat("\nfilepath:", filepath)
      cat("\nfilename:", filename)
      cat("\nrawdata number of bytes:", length(rawdata))
      cat("\nnumber of found text keys:",  nrow(keys))
      cat("\nnumber of assigned data fields:", length(data))
      cat("\ncurrent read position:", pos)
    }
    
    )
)