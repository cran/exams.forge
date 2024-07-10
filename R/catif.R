#' @rdname catif
#' @aliases condition_cat
#' @title Condition `cat`
#' @description Calls \code{cat} if \code{cond==TRUE}.
#'
#' @param cond logical: condition, if true then \code{cat} is called, otherwise not 
#' @param ... further parameters
#' @return Invisibly `cond`.
#' @export 
#'
#' @examples
#' catif(TRUE, "PDF")      # Should appear
#' catif(FALSE, "Moodle")  # Should not appear
catif <- function(cond, ...) {
  if (as.logical(cond)) cat(...)
  invisible(cond)
}

#' @rdname catif
#' @export
# condition_cat <- function(...){
#  catif(...)}
condition_cat <- catif
