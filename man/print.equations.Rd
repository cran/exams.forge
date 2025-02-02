% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equations.R
\name{print.equations}
\alias{print.equations}
\title{print.equations}
\usage{
\method{print}{equations}(x, ...)
}
\arguments{
\item{x}{an object used to select a method.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The data frame invisibly generated.
}
\description{
Prints an \code{equations} object with equations and variables. Internally, a data frame is generated, created and printed.
}
\examples{
# The equations describe the formulae for an confidence interval of the mean
e <- equations(o~x+c*s/sqrt(n), "v_o=\\\\bar{x}+c\\\\cdot\\\\frac{s^2}{n}", 
               u~x-c*s/sqrt(n), "v_u=\\\\bar{x}-c\\\\cdot\\\\frac{s^2}{n}", 
               e~c*s/sqrt(n),   "e  =c\\\\cdot\\\\frac{s^2}{\\\\sqrt{n}}",
               l~2*e,           "l  =2\\\\cdot e"                   
               )
print(e)
}
