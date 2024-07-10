#' @rdname binom_param
#' @title Binomial Parameters
#' @description Generates a data frame with potential values for `size` and `prob`, 
#' and is subjected to specific conditions:
#' \itemize{
#' \item If `length(mean) == 1` and it's an integer, it signifies the desired number of digits for the mean.
#' \item If `mean` is set to `NA` (the default), all means are permissible.
#' \item When `length(mean) > 1`, the product `size * prob` must be one of the valid means. 
#' \item The same rules applies to `sd`.
#' }
#' The parameters `norm` and `pois` can take on values of `NA`, `TRUE`, `FALSE`, 
#' or be defined as a function in the format: `function(size, prob)`. 
#' These values determine which `(size, prob)` combinations are eligible:
#' \itemize{
#' \item For `NA`, all combinations of `(size, prob)` are acceptable.
#' \item If specified as a function, only those combinations for which the function returns `TRUE` are considered valid.
#' \item If set to `TRUE`, combinations are accepted only if they satisfy either the condition `size * prob * (1 - prob) > 9` 
#' (for `norm`, indicating a normal distribution approximation), or the conditions `prob < 0.05` and `n > 10` 
#' (for `pois`, implying a Poisson distribution approximation).
#' \item If set to `FALSE`, the approximations should not hold for any combination.
#' }
#' Please be aware that there is no guarantee that the resulting data frame will include a valid solution.
#' 
#' @param n integer: vector number of observations
#' @param p numeric: vector of probabilities
#' @param mean integer or numeric: number of digits the mean should have
#' @param sd integer or numeric: number of digits the standard deviation should have
#' @param norm logical or function: normal approximation possible
#' @param pois logical or function: poisson approximation possible
#' @param tol numeric: the tolerance for numerical comparison (default: `1e-6)
#'
#' @return a data frame with possible choices of `n` , `p`, `mean` and `sd` 
#' @export
#'
#' @examples
#' binom_param(1000:50000, (5:25)/100, 0, 0)
#binom_param <- function(n, p, mean=NA, sd=NA, norm=NA, pois=NA) {
#  if (is.na(mean)) {
#    mean <- ceiling(mean(log10(c(min(n)*min(p), max(n)*max(p)))))
#    if (mean>0) mean <- 0 else mean <- 1-mean
#  }
#  mean <- round(mean)
#  if (is.na(sd)) sd <- mean
#  res <- list(n=numeric(0), p=numeric(0), mean=numeric(0), sd=numeric(0))
#  for (i in 1:length(p)) {
#    m   <- n*p[i]
#    s   <- sqrt(n*p[i]*(1-p[i]))
#    ind <- which(has_digits(m, mean) & has_digits(s, sd)) 
#    if (length(ind)) {
#      res$n <- c(res$n, n[ind])
#      res$p <- c(res$p, rep(p[i], length(ind)))
#      res$mean <- c(res$mean, m[ind])
#      res$sd   <- c(res$sd, s[ind])
#    }
#  }
#  res <- as.data.frame(res)
#  if (isTRUE(norm))  res <- res[res$sd>9,]
#  if (isFALSE(norm)) res <- res[res$sd<9,]
#  if (isTRUE(pois))  res <- res[(res$n>10) & (res$p<0.05),]
#  if (isFALSE(pois)) res <- res[(res$n<10) | (res$p>0.05),]
#  res
#}
binom_param <- function (n, p, mean = NA, sd = NA, norm = NA, pois = NA, tol=1e-6) {
  g <- expand.grid(n=n,p=p)
  g <- cbind(g, e=g[,1]*g[,2], v=g[,1]*g[,2]*(1-g[,2]))
  # check mean
  if (length(mean)==1) {
    cond <- if (is.na(mean)) rep(TRUE, nrow(g)) else equal(g[,3], round(g[,3], mean), tol=tol)
  } else {
    cond <- equal(g[,3], mean, outer=TRUE, tol=tol)
    amean <- mean
  }
  g    <- g[cond,]
  # check variance
  asd <- sqrt(g[,4])
  if (length(sd)==1) {
    cond <- if (is.na(sd)) rep(TRUE, nrow(g)) else equal(asd, round(asd, sd), tol=tol)
  } else {
    cond <- equal(asd, round(asd, sd), outer=TRUE, tol=tol)
  }
  g    <- g[cond,]
  # check norm
  if (is.function(norm)) cond <- norm(g[,1:2])
  if (is.logical(norm)) { 
    if (is.na(norm)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- if (norm) (g[,4]>9) else (g[,4]<9)  
    }
  }
  g    <- g[cond,]
  # check pois
  if (is.function(pois)) cond <- pois(g[,1:2])
  if (is.logical(pois)) { 
    if (is.na(pois)) {
      cond <- rep(TRUE, nrow(g))
    } else {
      cond <- if (pois) ((g[,1]>10) & (g[,2]<0.05)) else  ((g[,1]<10) | (g[,2]>0.05))
    }
  }
  g   <- g[cond,]
  data.frame(n=g[,1], p=g[,2], mean=g[,3], sd=sqrt(g[,4]))
}
