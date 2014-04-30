# Null default
# Analog of || from ruby
#
# @keyword internal
# @name nulldefault-infix
# @author Hadley Wickham
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Wrapper for parsing binary data.
#' 
#' Convenience wrapper for parsing binary data.
#' For more details on reading binary data, check ?readBin
#' 
#' @param type data type see \code{\link{map_binary_data_type}} for details
#' @param length how many instances of this object (for characters and raw this means length of string, all others a vector)
#' @return read data
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

#' Binary data type mapping
#' 
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

