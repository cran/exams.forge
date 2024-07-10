#' @rdname affix
#' @aliases add_affix
#' @aliases add_math
#' @aliases lmath
#' @aliases add_bracket
#' @aliases brkt
#' @aliases add_cdata
#' @aliases remove_affix
#' @aliases remove_quotes
#' @aliases remove_cdata
#' @aliases lmath
#' @title  Quote and Prefix and/or Suffix Manipulation
#' @description \code{affix} adds a prefix and/or a suffix to a (character) vector
#' 
#' @param txt vector: (character) vector to add a prefix and/or a suffix 
#' @param prefix character: prefix to add or delete (default: \code{""})
#' @param suffix character: suffix to add or delete (default: \code{""})
#'
#'
#' @return a character vector
#' @export
#'
#' @examples
#' x <- runif(5)
#' affix(x, "$", "$")
#' math(x)
affix <- function(txt, prefix='', suffix='')  { 
  paste0(prefix, txt, suffix) 
}

#' @rdname affix
#' @description \code{math} adds a \code{$} as pre- and suffix to a (character) vector
#' @export
math  <- function(txt) { 
  affix(txt, '$', '$') 
}

#' @rdname affix
#' @description \code{bracket} adds a \code{(} as prefix and \code{)} as suffix to a (character) vector
#' @export
bracket <- function(txt) { 
  affix(txt, '(', ')') 
}

#' @rdname affix
#' @description \code{unaffix} deletes a pre- and/or suffix to a (character) vector
#' @export
unaffix  <- function(txt, prefix='', suffix='')  { 
  index <- which(startsWith(txt, prefix))
  if (length(index)) txt[index] <- substring(txt[index], nchar(prefix)+1)
  index <- which(endsWith(txt, suffix))
  if (length(index)) txt[index] <- substring(txt[index], 1, nchar(txt)-nchar(suffix))
  txt
}

#' @rdname affix
#' @description \code{unquote} deletes double quotes at the beginning and the ending of a (character) vector
#' @export
unquote  <- function(txt) { unaffix(txt, '"', '"') }
  
#' @rdname affix
#' @description \code{uncdata} deletes a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
uncdata  <- function(txt) { unaffix(txt, '<![CDATA[', ']]>') }

#' @rdname affix
#' @description \code{cdata} adds a \code{<![CDATA[} as prefix and \code{]]>} as suffix
#' @export  
cdata  <- function(txt) { affix(txt, '<![CDATA[', ']]>') }

#' @rdname affix
#' @export
# add_affix <- function(...){
#  affix(...)}
add_affix <- affix

#' @rdname affix
#' @export
# add_cdata <- function(...){
#  cdata(...)}
add_cdata <- cdata

#' @rdname affix
#' @export
# add_math <- function(...){
# math(...)}
add_math <- math

#' @rdname affix
#' @export
# lmath <- function(...){
#  math(...)}
lmath <- math

#' @rdname affix
#' @export
# add_bracket <- function(...){
#  bracket(...)}
add_bracket <- bracket

#' @rdname affix
#' @export
# brkt <- function(...){
#  bracket(...)}
brkt <- bracket

#' @rdname affix
#' @export
# remove_affix <- function(...){
#  unaffix(...)}
remove_affix <- unaffix

#' @rdname affix
#' @export
# remove_quotes <- function(...){
#  unquote(...)}
remove_quotes <- unquote

#' @rdname affix
#' @export
# remove_cdata <- function(...){
#  uncdata(...)}
remove_cdata <- uncdata

