% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/html_matrix.R
\name{hm_cell}
\alias{hm_cell}
\alias{hm_index}
\alias{hm_row}
\alias{hm_col}
\alias{hm_title}
\alias{hm_tr}
\alias{hm_table}
\alias{modify_cell}
\alias{mod_cell}
\alias{modify_col}
\alias{mod_col}
\alias{modify_index}
\alias{mod_ind}
\alias{modify_row}
\alias{mod_row}
\alias{modify_table}
\alias{mod_t}
\alias{modify_title}
\alias{mod_title}
\alias{modify_tr}
\alias{mod_tr}
\title{\code{html_mmatrix} Modification}
\usage{
hm_cell(x, row = NULL, col = NULL, ..., byrow = FALSE)

hm_index(x, ind, ...)

hm_title(x, ...)

hm_table(x, ...)

hm_row(x, ind, ...)

hm_col(x, ind, ...)

hm_tr(x, ind, ...)

modify_cell(x, row = NULL, col = NULL, ..., byrow = FALSE)

mod_cell(x, row = NULL, col = NULL, ..., byrow = FALSE)

modify_col(x, ind, ...)

mod_col(x, ind, ...)

modify_index(x, ind, ...)

mod_ind(x, ind, ...)

modify_row(x, ind, ...)

mod_row(x, ind, ...)

modify_table(x, ...)

mod_t(x, ...)

modify_title(x, ...)

mod_title(x, ...)

modify_tr(x, ind, ...)

mod_tr(x, ind, ...)
}
\arguments{
\item{x}{an \code{html_matrix} object}

\item{row}{integer: row(s) to access}

\item{col}{integer: column(s) to access}

\item{...}{further elements}

\item{byrow}{logical: order indices by row or column (default: \code{FALSE})}

\item{ind}{integer vector or matrix: has access to various (row and columns) elements (first column: row, second column: column)}
}
\value{
A modified \code{html_matrix} object.
}
\description{
\itemize{
\item \code{hm_cell} or \code{hm_index} modify a data cell format (\code{fmt="\%s"}), value (unnamed parameter) or style (\code{text_align="left"})
\item \code{hm_col} or \code{hm_row} modify a row or column format (\code{fmt="\%s"}), value (unnamed parameter) or style (\code{text_align="left"})
\item \code{hm_title} modifies the title attribute of an \code{html_matrix} based on specific arguments
\item \code{hm_table} modifies the properties of the entire HTML table within an \code{html_matrix}
\item \code{hm_tr} modifies the properties of one or more table rows (tr elements) in an \code{html_matrix}. Row indices for modification (\code{ind}) can be specified along with additional parameters to customize the row format, values, or style
}
}
\examples{
l <- html_matrix(matrix(1:6, ncol=2))
# replace l[1,1] by NA
hm_cell(l, 1, 1, NA)
# replace l[1,1] by NA and set the text_align to center
hm_cell(l, 1, 1, NA, text_align="center")
# replace l[1,3] and l[2,1] by NA
rcind <- cbind(c(1,3), c(2, 1))
hm_index(l, rcind, NA)
# set a new title
hm_title(l, "new title")
# set a new row or column title
hm_row(l, 2, "row 2")
hm_col(l, 1, "col 1")
# set fmt by column or row
print(hm_cell(l, fmt=c("\%.0f", "\%.1f", "\%.2f"), byrow=FALSE), which="fmt")
print(hm_cell(l, fmt=c("\%.0f", "\%.1f"), byrow=TRUE), which="fmt")
}
