% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IrmsScanDataClass.R
\docType{class}
\name{IrmsScanData}
\alias{IrmsScanData}
\title{IrmsScanData reference class}
\description{
IrmsScanData reference class
}
\section{Fields}{

\describe{
\item{\code{massData}}{stores the scan data,inherited from IrmsDataClass}
}}
\section{Methods}{

\describe{
\item{\code{get_mass_data(masses = names(.self$plotOptions$masses), melt = FALSE, ...)}}{get the mass trace data for specific masses, can be provided in \code{melt = TRUE} long format
for easy use in ggplot style plotting}

\item{\code{init_irms_data()}}{initialize irms data container}

\item{\code{make_ggplot(...)}}{generate a ggplot object for the data in this IrmsData object}

\item{\code{plot(...)}}{plot data with standard plot functions (fast) to standard output}
}}
\seealso{
\link{IrmsData}
ggplot data
}

