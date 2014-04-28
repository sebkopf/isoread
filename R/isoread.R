#' R interface to IRMS (isotope ratio mass spectrometry) file formats typically used in stable isotope geochemistry.
#' 
#' See \code{\link{isoread}} for details on how to use.
#' 
#' @name isoread-package
#' @aliases isoreadinfo
#' @docType package
#' @title isoread package
#' @author Sebastian Kopf
NULL

#' @include IsodatHydrogenContinuousFlowFileClass.R
NULL

#' Read isotope data files
#' 
#' Reads isodat file(s) and returns the contents as file type specific
#' instances of \code{BinaryFile} / \code{IrmsDataClass} (extends both).
#' 
#' @param file path to the file(s) to read
#' @param type type of the files to be read
#' \itemize{
#'    \item{'H_CSIA'}{ = compound specific IRMS data for hydrogen isotopes}
#' }
#' @param ... parameters passed to the \code{load} and \code{process} functions of the IsodatFile objects
#' @return List of file \code{type} specific objects. 
#' \itemize{
#'    \item{'H_CSIA'}{ = instance(s) of \code{IsodatHydrogenContinuousFlowFile} which implements \code{IrmsContinuousFlowData}}.
#' }
#' If file names start with a number,
#' then the number is used as key in the list, otherwise the whole filename is the key.
#' If there is only one file, the object is returned directly.
#' @export
isoread <- function(files, type, ...) {
  typeClass <- switch(
    type,
    H_CSIA = 'IsodatHydrogenContinuousFlowFile',
    stop("not a currently supported file type: '", type, "'"))
  
  files <- as.list(files)
  names(files) <- sapply(files, function(i) sub('^(\\d+).*', '\\1', basename(i)), simplify=T) 
  objs <- lapply(files, function(file) {
    message("Reading file ", file)
    obj <- new(typeClass, file)
    obj$load(...)
    obj$process(...)
    obj$check_data(...)
    obj$cleanup(clean_chrom_data = FALSE, ...)
    obj
  })
  
  if (length(objs) == 1) objs <- objs[[1]]
  invisible(objs)
}

#' Reads all isodat files in a folder.
#' 
#' See \code{\link{isoread}} for paramter and return value details.
#' @export
isoread_folder <- function(folder, type, extension = '.cf', ...) {
  isoread(
    dir(folder, pattern = extension, full.names = TRUE, ignore.case = TRUE),
    type = type, ...)
}
