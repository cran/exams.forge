#' @importFrom stats qt
#' @rdname CImu_data
#' @aliases dcimu
#' @title Confidence Intervals
#' @description
#' The `CImu_data` function is designed for the generation of confidence intervals pertaining to a population mean `mu`. 
#' The function accommodates scenarios in which a dataset `x` is either provided or generated through a random sampling process 
#' from a normal distribution, with user-specified parameters such as a mean `mu` and a standard deviation `sigma`. 
#' Subsequently, the function computes essential statistical measures, including the sample mean `xbar` and the standard deviation `sd`. 
#' Confidence intervals for the population mean are then calculated at user-defined confidence levels (`conf.level`). 
#' The output is a structured list containing pertinent statistics, encompassing the mean, sample standard deviation, 
#' confidence intervals, and other relevant details.
#' 
#' @param x  numeric: vector of data values 
#' @param n numeric: length of the vector x (if `n<1` then `n=5` is used)
#' @param xbar numeric: sample mean
#' @param sd numeric: sample standard deviation
#' @param conf.level numeric: vector of confidence levels of the interval (default: `c(0.9, 0.95, 0.99)`)
#' @param mu numeric: true value of the mean 
#' @param sigma numeric: vector of possible variance
#' 
#' @return a list with
#' * `a` with `1-(1-conf.level)/2`
#' * `n` number observations if given
#' * `xbar` mean of observations if not given
#' * `mu` theoretical mean if given
#' * `sd` standard deviation of observations
#' * `sigma` theoretical standard deviation if given 
#' * `df` degrees of freedom if a `t` distribution is used
#' * `q` if `sigma=NULL` 
#' * `ss` either `sd` or `sigma`
#' * `e` margin of error (half of the length of the confidence interval(s))
#' * `l` length of the confidence interval(s)
#' * `v` endpoints of the confidence interval(s)
#' @export
#'
#' @examples
#' # with data
#' x <- rnorm(100)
#' CImu_data(x, conf.level=0.95)
#' # simulate data internally
#' CImu_data(n=100, conf.level=0.95, mu=0, sigma=1)
CImu_data <- function(x=NULL, n=length(x), xbar=NULL, sd=NULL, conf.level=c(0.9, 0.95, 0.99), mu=NULL, sigma=NULL) {
  if (is.null(x)) {
    if (n<1) n <- 5
    if (length(sigma)>1) sigma <- sample(sigma, 1)
    if (length(mu)>1) mu <- sample(mu, 1)
    x <- rnorm(n, mean=mu, sd=sigma)
  } 
  if (is.null(xbar)) xbar <- mean(x)
  if (is.null(sd)) sd <- sd(x)
  ret <- list(a=1-(1-conf.level)/2, n=length(x), xbar=xbar, mu=mu, sd=sd, sigma=sigma, df=NULL)
  if (is.null(sigma)) {
    ret$q  <- qnorm(ret$a)
    ret$ss <- ret$sd
  } else {
    ret$df <- length(x)-1
    ret$q  <- qt(ret$a, ret$df)
    ret$ss <- ret$sigma
  }
  names(ret$q) <- conf.level
  ret$e <- ret$q*ret$ss/sqrt(ret$n)
  ret$l <- 2*ret$e
  ret$v <- xbar+c(-ret$e, ret$e)
  ret
}
#' @rdname CImu_data
#' @export dcimu
# dcimu <- function(...){
#  CImu_data(...)}
dcimu <- CImu_data
