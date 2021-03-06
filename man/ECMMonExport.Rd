% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ECMMon.R
\name{ECMMonExport}
\alias{ECMMonExport}
\title{Export model and solution into CSV files.}
\usage{
ECMMonExport(
  ecmObj,
  directoryName,
  modelID,
  fileNamePrefix = paste0(modelID, "-")
)
}
\arguments{
\item{ecmObj}{An ECMMon object.}

\item{directoryName}{A directory name for the export. If \code{NULL} no files are written.}

\item{modelID}{A string.}

\item{fileNamePrefix}{A string.}
}
\value{
An ECMMon object or \code{ECMMonFailureSymbol}.
}
\description{
Exports the model elements and solution data frame into CSV files
with a specified prefix and in a specified directory.
}
\details{
The CSV files are written in the specified directory \code{directoryName}.
The file name prefix \code{fileNamePrefix} is concatenated to the generic file names:
\code{"longFormComputationSpecification.csv", "featureMatrix.csv", "timeCellsInterpretation.csv", "featureMatrixTimeSeries.csv"}.
The conversion into long form of the computation specification is considered to be
more convenient from a "model management" perspective.
The data to be exported is assigned to result's \code{$Value}.
}
