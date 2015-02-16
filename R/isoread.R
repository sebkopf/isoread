#' R interface to IRMS (isotope ratio mass spectrometry) file formats typically used in stable isotope geochemistry.
#' 
#' See \code{\link{isoread}} for details on how to use.
#' 
#' @name isoread-package
#' @docType package
#' @title isoread package
#' @author Sebastian Kopf
#' @author Max Lloyd
NULL

#' @include IsodatHydrogenContinuousFlowFileClass.R
#' @include IsodatDualInletFileClass.R
#' @include IsodatClumpedCO2FileClass.R
NULL

#' Read isotope data files
#' 
#' Reads isodat file(s) and returns the contents as file type specific
#' instances of \code{\link{BinaryFile}} and \code{\link{IrmsData}} (extends both).
#' 
#' @param file path to the file(s) to read
#' @param type type of the files to be read
#' \itemize{
#'    \item{'H_CSIA'}{ = compound specific IRMS data for hydrogen isotopes}
#' }
#' @param load_chroms whether to keep the chromatograms in the objects (otherwise only peak tables are kept)
#' @param ... parameters passed to the \code{load} and \code{process} functions of the IsodatFile objects
#' @return List of file \code{type} specific objects. 
#' \itemize{
#'    \item{'DI'}{ = instance(s) of a basic \code{\link{IsodatDualInletFile}} which implements \code{\link{IrmsDualInletData}}}.
#'    \item{'CO2_CLUMPED'}{ = instance(s) of the more specialized \code{\link{IsodatClumbedCO2File}} which extends \code{\link{IsodatDualInletFile}}}.
#'    \item{'H_CSIA'}{ = instance(s) of \code{\link{IsodatHydrogenContinuousFlowFile}} which implements \code{\link{IrmsContinuousFlowData}}}.
#' }
#' If file names start with a number,
#' then the number is used as key in the list, otherwise the whole filename is the key.
#' If there is only one file, the object is returned directly.
#' @export
isoread <- function(files, type, load_chroms = T, ...) {
  typeClass <- switch(
    type,
    H_CSIA = 'IsodatHydrogenContinuousFlowFile',
    DI = 'IsodatDualInletFile',
    CO2_CLUMPED = 'IsodatClumpedCO2File',
    stop("not a currently supported file type: '", type, "'"))
  
  files <- as.list(files)
  names(files) <- sapply(files, function(i) sub('^(\\d+).*', '\\1', basename(i)), simplify=T) 
  objs <- lapply(files, function(file) {
    message("Reading file ", file)
    obj <- new(typeClass, file)
    obj$load(...)
    obj$process(...)
    obj$check_data(...)
    obj$cleanup(clean_chrom_data = !load_chroms, ...)
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

# =========== NOT unit tested yet!!!

#' Map peak table
#' 
#' Map peak table data of IrmsContinuousFlowData object(s) based on a data
#' frame or input excel file. 
#' 
#' @param iso IrmsContinuousFlowData object(s)
#' @param map either a data frame with a map (containing column Rt and Component)
#' or the file path to a mapping file, extension determines how it will be processed
#' currently only xlsx and xls are supported. Excel file must include column headers
#' on the indicated row (default startRow = 3) with columns Rt and Component Component
#' @param libfile name of the library file, also only xlsx and xls currently supported
#' if provided, will attempt to merge the components in the mapping file with the library
#' information for additional details on Formula and other compound properties
#' @export
map_peaks <- function(iso, map, startRow = 3, libfile = NULL, colClasses = c("numeric", "character", "character"), ...) {
  library(xlsx)
  types <- sapply(c(iso), class)
  if (!all(sapply(types, function(i) extends(i, "IrmsContinuousFlowData"))))
    stop("not an IrmsContinuousFlowData object, don't know how to apply a map to it")
  
  if (!is(map, "data.frame"))
    map <- read.xlsx2(map, 1, startRow = startRow, stringsAsFactors = F, header=TRUE, colClasses = colClasses, ...) 
  keys <- c("Rt", "Component")
  if (!all(keys %in% names(map)))
    stop("Missing either Rt or Component column, don't know what to do with this map")
  
  if (!is.null(libfile)) {
    lib <- read.xlsx2(libfile, 1, startRow = 3, stringsAsFactors = F, header=TRUE, colClasses = c("character"), ...) 
    map <- merge(map, lib, by.x = "Component", by.y = "Component", all.x = TRUE)
  }
  
  if ("Formula" %in% names(map))
    keys <- c(keys, "Formula")
  
  # apply map
  invisible(sapply(c(iso), function(i) {
    message("Mapping ", i$filename, " which has ", nrow(i$get_peak_table()), " peaks (mapping file has ", nrow(map) ,").")
    i$map_peaks(map[keys])
  }))
}

#' Summarize a collection of IrmsData objects
#' 
#' @param iso IrmsData object(s)
#' @param ... all passed to summarize
#' @export
summarize_all <- function(iso, ...) {
  types <- sapply(c(iso), class)
  if (!all(sapply(types, function(i) extends(i, "IrmsData"))))
    stop("not an IrmsData object, don't know how to summarize it")
  invisible(sapply(c(iso), function(i) { 
    i$summarize(...)
  }))
}

#' Reload an isodat file object.
#' 
#' Reload an existing isodat object with all the chromatographic data and
#' resets the peak table if keep_peaks = FALSE. Good for interrogation of
#' an individual file. Requires the original file to still be in the same
#' location.
#' 
#' @param iso the object to reload (can be a list)
#' @param remap_peaks whether to keep the peak identification or not
#' @param load_chroms whether to load the chroms (much smaller object without)
#' @return the reloaded obj (or list of objs)
#' @note currently only for type = "H_CSIA"
#' @export
reload <- function(iso, remap_peaks = TRUE, load_chroms = TRUE) {
  iso <- c(iso) # make it a list if it isn't already
  types <- sapply(iso, class)
  if (!all(sapply(types, function(i) extends(i, "IrmsContinuousFlowData"))))
    stop("not an IrmsContinuousFlowData object, don't know how to relaod")
  
  maps <- sapply(iso, function(i) list(subset(i$get_peak_table(), Component != " - ", select = c("Rt", "Component", "Formula"))))
  
  files <- vapply(iso, function(i) file.path(i$filepath, i$filename), FUN.VALUE = character(1))
  
  # FIXME: get the H_CSIA somewhere from the files themselves!!
  new_iso <- c(isoread(files, type = "H_CSIA"))
  
  
  if (remap_peaks)
    sapply(1:length(new_iso), function(i) 
      if (nrow(maps[[i]]) > 0) new_iso[[i]]$map_peaks(maps[[i]]))
  
  if (!load_chroms)
    sapply(new_iso, function(i) i$cleanup(clean_chrom_data = T))
  
  if (length(new_iso) == 1) 
    new_iso <- new_iso[[1]]
  invisible(new_iso)
}

#' File quickview
#'
#' This functions serves to gain a quick view of a loaded isodat file.
#' It shows the masses plot and prints a minimal subset of the peak table. Optionally reloads
#' the file (tries to keep the peak definitions).
#' @param iso a single isodat file obj
#' @param reload whether to reload the file (this is forced if there is no chromatographic data)
#' @param show list of peak table columns to show
#' @export
quickview <- function(iso, reload = FALSE,
                      show = c("Peak Nr.", "Status", "Ref. Peak", "Component", "Rt", "Start", "End", "Ampl. 2", "d 2H/1H")) {
  if (reload || nrow(iso$chromData) == 0) 
    iso <- reload(iso, remap_peaks = T, load_chroms = T)
  print(iso$make_ggplot(ratios=c()))
  print(iso$get_peak_table()[show])
  invisible(iso)
}