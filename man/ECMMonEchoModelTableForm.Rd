% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ECMMon.R
\name{ECMMonEchoModelTableForm}
\alias{ECMMonEchoModelTableForm}
\title{Echo model table form}
\usage{
ECMMonEchoModelTableForm(
  ecmObj,
  part = NULL,
  echoQ = TRUE,
  dataFrameResultQ = FALSE
)
}
\arguments{
\item{ecmObj}{An ECMMon object.}

\item{part}{A string that which part of the model to echo.
If NULL all parts are echoed.}

\item{dataFrameResultQ}{Should the result be a data frame or not?}
}
\value{
An ECMMon object
}
\description{
If there is a multi-site models echoes (parts of) that model.
Otherwise echoes the single-site model.
}
\details{
A list of data frames is assigned to \code{ecmObj$Value}.
}
\concept{Query functions}
