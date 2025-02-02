% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hyper_param.R
\name{hyper_param}
\alias{hyper_param}
\title{Parameters for Hypergeometric Distributions}
\usage{
hyper_param(
  m,
  n,
  k,
  mean = NA,
  sd = NA,
  norm = NA,
  pois = NA,
  binom = NA,
  tol = 1e-06,
  hyper2 = FALSE
)
}
\arguments{
\item{m}{numeric: the number of white balls in the urn}

\item{n}{numeric: the number of black balls in the urn}

\item{k}{numeric: the number of balls drawn from the urn, hence must be in \eqn{0, 1, ..., m+n}}

\item{mean}{integer or numeric: number of digits the mean should have}

\item{sd}{integer or numeric: number of digits the standard deviation should have}

\item{norm}{logical or function: normal approximation possible}

\item{pois}{logical or function: poisson approximation possible}

\item{binom}{logical or function: binomial approximation possible}

\item{tol}{numeric: the tolerance for numerical comparison (default: `1e-6)}

\item{hyper2}{logical: should the standard R parametrization \verb{(m, n, k)} be used or \verb{(n+m, m, k)}?}
}
\value{
A data frame with possible the choices of \code{n} , \code{p}, \code{mean} and \code{sd}.
}
\description{
Generates a data frame with potential values for \code{m}, \code{n} and \code{k}. If \code{hyper2} is \code{FALSE} then the
parametrization of \code{\link[stats:Hypergeometric]{stats::dhyper()}} is used, otherwise \code{n+m}, \code{m} and \code{k} is used and transformed to \code{m}, \code{n} and \code{k}.
In accordance with specific conditions it holds that:
\itemize{
\item if \code{length(mean)==1} and it's an integer, it signifies the desired number of digits for the mean
\item if \code{mean} is set to \code{NA} (the default), all means are permissible
\item when \code{length(mean) > 1}, the product \eqn{k*m/(n+m)} must be one of the valid means
\item the same rules apply to \code{sd}
}
The parameters \code{norm}, \code{pois} and \code{binom} can take on the values \code{NA}, \code{TRUE}, \code{FALSE},
or be defined as a function of the format: \verb{function(m, n, k)}.
These values determine which \verb{(m, n, k)} combinations are eligible:
\itemize{
\item for \code{NA}, all combinations of \verb{(m, n, k)} are acceptable
\item if specified as a function, only those combinations for which the function evaluates to \code{TRUE} are considered valid
\item if set to \code{TRUE}, combinations are accepted only if they satisfy either the condition \eqn{k*m/(m+n)*(1-m/(m+n))>=9}
(for \code{norm}, indicating a normal distribution approximation), the conditions \eqn{k/(n+m) < 0.05}, \eqn{m/(n+m) < 0.05} and \eqn{k>10}
(for \code{pois}, implying a Poisson distribution approximation) and the condition \eqn{k/(n+m) < 0.05} (for \code{binom},
implying a binomial distribution approximation)
\item if set to \code{FALSE}, the approximations should not hold for any combination.
}
Please be aware that there is no guarantee that the resulting data frame will include a valid solution.
}
\examples{
hyper_param(7:14, 1:13, 3:10, norm=FALSE, pois=FALSE, binom=FALSE, hyper2=TRUE)
}
