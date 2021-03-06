% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generics.R, R/InputData.R
\name{ReadFrom10X_h5}
\alias{ReadFrom10X_h5}
\alias{ReadFrom10X_h5,BRIC-method}
\title{Read 10X HDF5 file based on Seurat package}
\usage{
ReadFrom10X_h5(input = NULL, use.names = TRUE, unique.features = TRUE)

ReadFrom10X_h5(input = NULL, use.names = TRUE, unique.features = TRUE)

\S4method{ReadFrom10X_h5}{BRIC}(input = NULL, use.names = TRUE, unique.features = TRUE)
}
\arguments{
\item{Input}{input 10X Chromium output data with the extented name as ".h5"}

\item{...}{inherit function from Seurat::Read10X_h5}
}
\value{
The output from \code{\link{Read10X_h5}}
}
\description{
Read 10X HDF5 file based on Seurat package
}
