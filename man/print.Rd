% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.html_matrix.R
\name{print.html_matrix}
\alias{print.html_matrix}
\title{Print \code{html_matrix}}
\usage{
\method{print}{html_matrix}(x, ..., which = "")
}
\arguments{
\item{x}{an html_matrix object}

\item{...}{further parameters}

\item{which}{character: which component to print (default: \code{""})}
}
\value{
An invisible character matrix.
}
\description{
Prints an HTML matrix content or its components.
}
\examples{
m <- matrix(1:6, ncol=2)
l <- html_matrix_sk(m, title="1 to 6", fmt=rep("\%f",ncol(m)))
print(l, which=NA)      # returns full style information
print(l, which="fmt")   # returns format information
print(l, which="value") # identical to print(l)
}
