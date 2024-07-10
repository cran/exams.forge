#' @rdname as_string
#' @aliases lstring
#' @aliases as_obs
#' @aliases lobs
#' @aliases as_sum
#' @aliases as_fraction
#' @aliases lfrac
#' @title  Vector to String Conversion
#' @description Converts a character vector into a single string.
#' 
#' @param txt vector: (character) vector to merge 
#' @param val numeric: values to convert into fractions
#' @param collapse character: glues text between elements (default: \code{", "})
#' @param last character: glues text between the two last elements (default: \code{", and "})
#' @param name character: observation name (default: \code{"x"})
#' @param latex logical use of LaTeX \code{\\frac{.}{.}} or not (default: \code{FALSE})
#' @param sorted logical: sorted or not sorted observations  (default: \code{FALSE})
#' @param ... further parameters (given from \code{as_obs} to \code{as_string})
#'
#' @return a string 
#' @export
#'
#' @examples
#' x <- runif(5)
#' y <- c(TRUE, FALSE, NA) 
#' as_string(x)
#' as_string(y)
#' # toString
#' as_string(as.character(x))
#' as_string(as.character(y))
#' #
#' as_obs(x)
#' as_obs(sort(x), sorted=TRUE)
#' #
#' x <- round(runif(5), 2)
#' as_fraction(x)
#' as_fraction(x, TRUE)
#' #
#' y <- round(runif(5), 2)
#' as_sum(x)
as_string <- function (txt, collapse=", ", last=", and ") {
  n <- length(txt)-1
  collapse <- c(rep(collapse, length.out=n), '')
  if (is.character(last)) collapse[n] <- last
  paste0(paste0(txt, collapse), collapse="")
}

#' @rdname as_string
#' @export
as_sum <- function (txt) { as_string(txt, collapse="+", last="+") }

#' @rdname as_string
#' @export
as_obs <- function(txt, name="x", sorted=FALSE, ...) {
  txt <- paste0('$', name, '_{', if(sorted) '(' else '', 1:length(txt), if(sorted) ')' else '', '}=', txt, '$')
  as_string(txt, ...)
}

#' @rdname as_string
#' @export
as_fraction <- function(val, latex=FALSE, sorted=FALSE, ...) {
  pfrac <- function(v) {
    if (length(v)==1) return(v)
    paste0("\\frac{", v[1], "}{", v[2], "}")    
  }
  #
  if (sorted) val <- sort(val)
  f  <- fractions(val)
  ff <- attr(f, "fracs")  
  if (latex) ff <- sapply(strsplit(ff, "/"), pfrac)
  ff
}

#' @rdname as_string
#' @export
# lobs <- function(...){
#  as_obs(...)}
lobs <- as_obs

#' @rdname as_string
#' @export
# lstring <- function(...){
#  as_string(...)}
lstring <- as_string

#' @rdname as_string
#' @export
# lfrac <- function(...){
#  as_fraction(...)}
lfrac <- as_fraction
