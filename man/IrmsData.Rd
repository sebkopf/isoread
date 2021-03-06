% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IrmsDataClass.R
\docType{class}
\name{IrmsData}
\alias{IrmsData}
\alias{get_data_table,}
\alias{make_ggplot}
\title{IrmsData reference class}
\arguments{
\item{plotOptions}{list of custom plotOptions
check internal consistency of data
get data for masses}

\item{masses}{which masses to retrieve, all defined ones by default}

\item{melt}{whether to melt the data frame
plot data
ggplot data
export data to csv}
}
\description{
IrmsData reference class
}
\section{Fields}{

\describe{
\item{\code{plotOptions}}{holds information about default plotting options}

\item{\code{massData}}{stores the raw mass data}

\item{\code{dataTable}}{stores the processed dataTable}

\item{\code{dataTableColumns}}{stores the definition of which columns exist in the
data table and what their proper data types are
constructor}
}}
\section{Methods}{

\describe{
\item{\code{export_data(file, ...)}}{export the data stored in this object to file}

\item{\code{get_data_table(...)}}{retrieve the data table}

\item{\code{get_mass_data(masses = names(.self$plotOptions$masses), melt = FALSE, ...)}}{get the mass trace data for specific masses, can be provided in \code{melt = TRUE} long format
for easy use in ggplot style plotting}

\item{\code{init_irms_data()}}{initialize irms data container}

\item{\code{make_ggplot(...)}}{generate a ggplot object for the data in this IrmsData object}

\item{\code{plot(...)}}{plot data with standard plot functions (fast) to standard output}

\item{\code{set_plot_options(...)}}{set plot options, e.g.: \code{set_plot_options(labels = list(xmasses = 'Cycle', ymasses = 'Signal [mV]'))}}

\item{\code{summarize(file, ....)}}{summarize the data stored in this object and save it to file}
}}

