% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grouped_data.R
\name{grouped_data}
\alias{grouped_data}
\alias{grouped_stats}
\alias{dgrouped}
\title{Central Tendency Measures' Computation of Grouped Data}
\usage{
grouped_data(x, n, compute = c("mean", "fine", "coarse"), tol = 1e-06)

grouped_stats(x, n, compute = c("mean", "fine", "coarse"), tol = 1e-06)

dgrouped(x, n, compute = c("mean", "fine", "coarse"), tol = 1e-06)
}
\arguments{
\item{x}{numeric: borders}

\item{n}{numeric: absolute frequencies for each group}

\item{compute}{numeric/character: coefficient to compute}

\item{tol}{numeric: tolerance for numerical comparison}
}
\value{
A list with the class, result and a table.
}
\description{
Computes mean, mode or quantile/median of grouped data.
}
\examples{
x <- 1:4
n <- ddiscrete(runif(3))
grouped_data(x, n)
}
