#' @include IrmsDataClass.R
NULL

#' Convenience function to export data from multiple
#' IrmsData objects of the same class into a comma-separated value file.
#' 
#' @export
export_data <- function(data, file = "irms_data_export.csv", ...) {
  if (!is(data, 'list')) data <- list(data)
  
  if (!all(sapply(data, function(i) extends(class(i), 'IrmsData'))))
    stop("Not all data are IrmsData objects. Classes: ", paste(sapply(data, class), collapse = ", "))
  
  if (!all(sapply(data, function(i) class(i) == class(data[[1]]))))
    stop("Not all data are the same kind of IrmsData object. Classes: ", paste(sapply(data, class), collapse = ", "))
  
  message("Exporting data for ", length(data), " IrmsData objects")
  data[[1]]$export_data(file = file, headers = TRUE, ...)
  sapply(data[-1], function(i) i$export_data(file = file, headers = FALSE, append = TRUE, ...))
  invisible(NULL)
}