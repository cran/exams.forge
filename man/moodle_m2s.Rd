% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodle_m2s.R
\name{moodle_m2s}
\alias{moodle_m2s}
\alias{mchoice_moodle}
\title{Moodle Multiple-Choice}
\usage{
moodle_m2s(file, newfile = NULL, verbose = 1)

mchoice_moodle(file, newfile = NULL, verbose = 1)
}
\arguments{
\item{file}{character: Moodle XML file with exercises to read from}

\item{newfile}{character:  Moodle XML file to write in (default: \code{file})}

\item{verbose}{integer: output generation (default: \code{1})}
}
\value{
Invisibly, the written file name.
}
\description{
The \code{exams} package does not support multiple-choice questions with multiple correct answers; it only allows for one answer to be chosen.
However, Moodle does support such questions. The function reads the XML file generated by \code{exams.forge} and makes changes
for all \code{mchoice} questions:
\itemize{
\item \code{<single>...</single>} to \code{<single>true</single>}, and
\item modifies the attribute \code{fraction} in the tags \code{<answer fraction="...">...</answer>}.
If \code{fraction} is less than 0, it is set to zero, and if \code{fraction} is greater than 0, it is set to 100.
}

If the \code{file} does not end with \code{.xml}, then \code{.xml} is appended. At the end, the modified XML code is stored in \code{newfile}.
}
\examples{
if (interactive()) {
  newfile <- tempfile(fileext=".xml")
  moodle_m2s(system.file("xml", "klausur-test.xml", package="exams.moodle"), newfile=newfile)
  file.edit(newfile)
}
}
