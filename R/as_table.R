#' @rdname as_table
#' @aliases toTable
#' @title Conversion to Table
#' @description Converts a vector into a horizontal table. 
#'
#' @inheritParams xtable::xtable
#' @param ... further parameters for \code{print.xtable}
#'
#' @return A string.
#' @importFrom xtable xtable print.xtable
#' @export
#'
#' @examples
#' x <- runif(5)
#' tab <- vec2mat(x, colnames=1:length(x))
#' as_table(tab)
as_table <- function(x, caption = NULL, label = NULL, align = NULL, digits = NULL,
                     display = NULL, auto = FALSE, ...) {
  xt <- xtable(x, caption = caption, label = label, align = align, digits = digits,
               display = display, auto = auto)
  xt <- print(xt, ...)
  strsplit(xt, "\n", fixed=TRUE)[[1]]
}

#' @rdname as_table
#' @export
# toTable <- function(...){
#  as_table(...)}
toTable <- as_table
