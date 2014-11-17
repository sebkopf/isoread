#' Binary File reference class
#' @name BinaryFile
#' @exportClass BinaryFile
#' @field filepath stores the path to the binart file
#' @field filename stores the filename
#' @field creation_date stores the date the file was created (if it could be retrieved,
#' which is not always the case when running on linux but no problem on OS X and windows)
#' @field rawdata this is the binary raw data from the file (typically removed during cleanup
#' unless clean_raw = FALSE)
#' @field keys these are the Unicode and ASCII text fragments found in the binary file,
#' they are used for navigating in the file when pulling out the relevant data (typically
#' removed during cleanup unless clean_keys = FALSE)
#' @field data a list that contains all the actual data pulled from the file
BinaryFile <- setRefClass(
  "BinaryFile",
  fields = list (
      filepath = 'character',
      filename = 'character',
      creation_date = 'POSIXct', # file creation date
      rawdata = 'raw',
      keys = 'data.frame',
      data = 'list',
      pos = 'integer' # current position in data stream
    ),
  methods = list(
    initialize = function(file, ...){
      "initialize BinaryFile object, requires a file path"
      if (!missing(file))
        initFields(
          filename = basename(file),
          filepath = dirname(file))
      callSuper(...)
    },
    
    load = function(...) {
      "load the data from the file and generate key lookup"
      read_file()
      find_keys()
    },
    
    process = function(...) {
      "process the raw data to fill the data list"
      if (length(rawdata) == 0)
        stop("no data available, make sure load() was called and the binary file ",
             "is properly loaded (for example, have a look at the keys field)")
    },
    
    #' cleanup the object
    cleanup = function(clean_raw = TRUE, clean_keys = TRUE, ...) {
      "clean up the object by removing the raw data and keys (and other large but only transiently important information) from memory"
      if (clean_raw)
        rawdata <<- raw()
      if (clean_keys)
        keys <<- data.frame()
    },
    
    parse = function(type, length = 1, id = NA, skip_first = 0) {
      " parse binary data at current position in the data stream
      advances pointer by the size of the read data 

      #' @param type see \\code{\\link{map_binary_data_type}}
      #' @param length see \\code{\\link{parse_binary_data}}
      #' @param id if provided, will store the parsed data with this key in the \\code{$data} field
      #' @param skip_first how many bytes to skip before reading this
      "
      
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
    
    # IMPLEMENT skip_after_each for easy recuriing motive recognition
    # current problem with that is that it loops through by type (=col)
    parse_array = function(types, n, id = NA, skip_first = 0) {
      "repeatedly read the same set of information into a data frame

      #' @param types a named vector of data types (for data types see \\code{\\link{parse_binary_data}}), 
      #' the names are used for the columns of the resulting data frame
      #' @param id if provided, will store the parsed data with this key in the $data field
      #' @param n length of array
      #' @param skip_first how many bytes to skip before reading this"
      
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
    
    skip = function(nbyte) {
      "skip nbyte number of bytes in the raw data stream"
      pos <<- as.integer(pos + nbyte)
    },
    
    find_key = function(pattern, occurence = NULL, fixed = FALSE, byte_min = 0, byte_max = length(rawdata)) {
      "finds all keys matching 'key' or a specific occurence of it (use -1 for last occurence)
      #' @param fixed whether to find the key(s) by regexp match or fixed string (default = pattern)
      #' @param byte_min only look for keys that start after this position
      #' @param byte_max only look for keys that start before this position
      #' @return the lines of the keys data frame with all the information about the found key(s)"
      
      if (nrow(keys) == 0)
        stop("no keys available, make sure load() was called")
      
      sub_keys <- subset(keys, byteStart > byte_min & byteStart < byte_max)
      if (nrow(sub_keys) == 0)
        stop("no keys in this byte interval: ", byte_min, " - ", byte_max)
      
      if (length(idx <- grep(pattern, sub_keys$value, fixed = fixed)) == 0)
        stop("key '", pattern, "' was not found")
      
      if (!is.null(occurence)) {
        if (occurence == -1) occurence <- length(idx)
      
        if (occurence > length(idx))
          stop("key '", key, "' was found but only has ", length(idx), " occurences ",
               "(trying to select occurence #", occurence, ")")
      } else {
        occurence <- 1:length(idx) # return ALL found occurences
      }
      
      return(sub_keys[idx[occurence], , drop=F])
    },
    
    move_to_key = function(key, occurence = 1, fixed = TRUE) {
      "moves position to the end of a specific key or occurence of a key 
      #' @param key either a string or a data.frame line with key value and byteEnd (the way it is returned by find_key)
      #' @param occurence if key is a string, which occurence to move to? (use -1 for last occurence)
      #' @param fixed whether to find the key (if a string) by regexp match or fixed string (default = fixed string)"
      
      if (is(key, "character")) key <- find_key(key, occurence, fixed)
      else if (is(key, "list")) key <- as.data.frame(key)
        
      if (!is(key, "data.frame") || nrow(key) != 1 || !("byteEnd" %in% names(key))) 
        stop("not a valid key entry, can't move there: ", key)
      pos <<- as.integer(key["byteEnd"]) + 1L
    },
    
    read_file = function(){
      "read the binary file
      
      #' @note this does not work for very large files probably because of the 2^31-1 
      #' limit on vector size! think about ways to fix this...
      #' --> might have to acually read directly from the conection instead of the raw data buffer!"
      
      path <- file.path(filepath, filename)
      if (is.na(size <- file.info(path)$size) || file.info(path)$isdir == TRUE)
        stop("file does not exist: ", path)
      
      creation_date <<- switch(
        .Platform$OS.type, #  Sys.info()[['sysname']] will provide Windows, Darwin and Linux instead
        windows = {
          # Windows
          file.info(path)$ctime
        },
        unix = {
          # Linux and OS X, try it with stat, otherwise just throw a warning and use mdate
          bdate <- NULL
          tryCatch({
            cmd <- paste0('stat -f "%DB" "', path, '"') # use BSD stat command
            ctime_sec <- as.integer(system(cmd, intern=T)) # retrieve birth date in seconds from start of epoch (%DB)
            bdate <- as.POSIXct(ctime_sec, origin = "1970-01-01", tz = "") # convert to POSIXct
          }, error = function(e) warning(e)) # throw errors as warnings
          if (is.null(bdate))
            bdate <- file.info(path)$mtime #FIXME: currently returning last modification time instead if there's any trouble
          bdate
        }, 
        stop("don't know how to get file birth date on platform ", .Platform$OS.type))
      
      # read
      con <- file(path, "rb")
      rawdata <<- readBin(con, raw(), n = size)
      close(con)
    },
    
    find_keys = function(asciiL = 10, unicodeL = 5) {
      "finds all unicode and ascii strings and stores them for navigation around the file"
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
    
    
    clean_keys = function(removeText = NULL, removePattern = NULL, unlessByteLength = 0, unlessText = NULL) {
      "clean up keys by removing randomly found strings that are clearly not proper targets"
      
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
    
    get_info = function(show = c()) {
      "Get basic information about the object"
      rbind(
        data.frame(Property = c("File location", "Date"), 
                   Value = c(file.path(filepath, filename), format(creation_date))),
        data.frame(Property = names(data), 
                   Value = vapply(data, 
                                  function(i) if (is(i, "data.frame")) "data frame (not shown)" else as.character(i[1]), 
                                  FUN.VALUE = character(1), USE.NAMES = FALSE))
      )
    },
    
    show = function() {
      #show some useful summary info about this binary file class"
      cat("\nBinary File information:")
      cat("\nrawdata number of bytes:", length(rawdata))
      cat("\nnumber of found text keys:",  nrow(keys))
      cat("\nnumber of assigned data fields:", length(data))
      cat("\ncurrent read position:", pos)
      cat("\ndata information:\n")
      print(get_info())
    }
    
    )
)