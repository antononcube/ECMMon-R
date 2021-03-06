% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ECMMon.R
\name{ECMMonMemberPresenceCheck}
\alias{ECMMonMemberPresenceCheck}
\title{General member presence check.}
\usage{
ECMMonMemberPresenceCheck(
  ecmObj,
  memberName,
  memberPrettyName = memberName,
  functionName = "",
  logicalResultQ = FALSE,
  warningQ = TRUE
)
}
\arguments{
\item{ecmObj}{An ECMMon object.}

\item{memberName}{The name of the member to be checked.}

\item{memberPrettyName}{A pretty member name (for messages).}

\item{functionName}{The name of the delegating function.}

\item{logicalResultQ}{Should the result be a logical value or not?}

\item{warningQ}{Should warning message be issued or not?}
}
\value{
A logical value or an ECMMon object.
}
\description{
A general function for checking the presence of a data member in an ECMMon object.
}
