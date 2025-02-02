% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knitif.R
\name{knitif}
\alias{knitif}
\alias{knit_select}
\title{Knitting a Text Argument}
\usage{
knitif(n, ..., envir = knit_global())

knit_select(n, ..., envir = knit_global())
}
\arguments{
\item{n}{character: text argument to use}

\item{...}{character: arguments to choose from}

\item{envir}{environment: in which code chunks are to be evaluated (default: \verb{[knitr::knit_global]})}
}
\value{
A character.
}
\description{
Selects a text argument and returns the knitted result.
}
\examples{
knitif(runif(1)<0.5, 'TRUE'="`r pi`", 'FALSE'="$\\\\pi=`r pi`$")
}
