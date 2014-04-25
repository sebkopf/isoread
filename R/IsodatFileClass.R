#' @include BinaryFileClass.R
NULL

#' Isodat file class
#' @note not entirely sure yet what will be abstracted into this class
#' @rdname IsodatFile
IsodatFile <- setRefClass(
  "IsodatFile",
  contains = "BinaryFile",
  fields = list (),
  methods = list()
)