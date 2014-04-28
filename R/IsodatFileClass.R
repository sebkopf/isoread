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
  methods = list()
)