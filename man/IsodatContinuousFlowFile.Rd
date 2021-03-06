% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IsodatContinuousFlowFileClass.R
\docType{class}
\name{IsodatContinuousFlowFile}
\alias{IsodatContinuousFlowFile}
\title{Continous Flow DataClass}
\arguments{
\item{read_mass_data}{whether to read chromatographic mass data (can be a lot of data)
custom show function to display roughly what data we've got going}
}
\description{
Objects of this class hold the isotopic data from continuous flow information
recorded in Isodat files (.dxf files) currently only tested with Isodat 3.0 files).
}
\details{
This class is derived from \link{IrmsContinuousFlowData} which defines a number
of useful plotting, export and data access methods. This class also derived
\link{BinaryFile} which provides functionality for interacting with the
underlying \link{IsodatFile}.
}
\section{Methods}{

\describe{
\item{\code{cleanup(clean_raw = TRUE, clean_keys = TRUE, ...)}}{clean up the object by removing the raw data and keys (and other large but only transiently important information) from memory}

\item{\code{get_info(show = c())}}{Get basic information about the object}

\item{\code{initialize(file, ...)}}{initialize BinaryFile object, requires a file path}

\item{\code{process(...)}}{process the raw data to fill the data list}
}}
\seealso{
\link{BinaryFile}, \link{IsodatFile}, \link{IrmsContinuousFlowData}, \link{IrmsData}
initialize
initialize irms data container
}

