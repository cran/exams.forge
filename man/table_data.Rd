% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_data.R
\name{table_data}
\alias{table_data}
\alias{freq_table}
\alias{dtable}
\title{Frequency Table}
\usage{
table_data(
  nrow,
  ncol,
  unit = 10,
  maxit = 1000,
  n = 100,
  m2 = ceiling(log(n)/log(2)),
  m5 = ceiling(log(n)/log(5))
)

freq_table(
  nrow,
  ncol,
  unit = 10,
  maxit = 1000,
  n = 100,
  m2 = ceiling(log(n)/log(2)),
  m5 = ceiling(log(n)/log(5))
)

dtable(
  nrow,
  ncol,
  unit = 10,
  maxit = 1000,
  n = 100,
  m2 = ceiling(log(n)/log(2)),
  m5 = ceiling(log(n)/log(5))
)
}
\arguments{
\item{nrow}{integer: number of rows}

\item{ncol}{integer: number of columns}

\item{unit}{integer: reciprocal of smallest non-zero probability (default: \code{10})}

\item{maxit}{integer: maximal number of iterations (default: \code{1000})}

\item{n}{integer: maximal sum of table entries (default: \code{100})}

\item{m2}{integer: maximal power of two used on normalized the table (default: \code{ceiling(log(n)/log(2))})}

\item{m5}{integer: maximal power of five used on normalized the table  (default: \code{ceiling(log(n)/log(5))})}
}
\value{
A frequency table where all entries can be written as \eqn{2^{p_{ij}} 5^{q_{ij}}}.
}
\description{
Creates a frequency table where all entries can be written as \eqn{2^{p_{ij}} 5^{q_{ij}}}. It holds that \eqn{p_{ij}<m2} and  \eqn{q_{ij}<m5}.
If the algorithm does not find a solution, then an error is thrown. Try to increase \code{unit} to 20, 50, 100 and so on.
Once a table is found, the table is normalized by dividing all entries by a number such that the entries are still integer.
Finally, a multiplicator of the form \eqn{2^p 5^5} is randomly chosen, ensuring that the sum of the entries is less than, or equal to \code{n}.
}
\examples{
tab22 <- table(2, 2)
tab22
divisor_25(tab22)
nom.cc(tab22)         # Should be zero
#
table(3, 2)
table(4, 2)
}
