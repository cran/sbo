% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{preprocess}
\alias{preprocess}
\title{Preprocess text corpus}
\usage{
preprocess(input, erase = "[^.?!:;'\\\\w\\\\s]", lower_case = TRUE)
}
\arguments{
\item{input}{a character vector.}

\item{erase}{a length one character vector. Regular expression matching parts of
text to be erased from input. The default removes anything not alphanumeric,
white space, apostrophes or punctuation characters (i.e. ".?!:;").}

\item{lower_case}{a length one logical vector. If TRUE, puts everything to lower
case.}
}
\value{
a character vector containing the processed output.
}
\description{
A simple text preprocessing utility.
}
\examples{
preprocess("Hi @ there! I'm using `sbo`.")
}
\author{
Valerio Gherardi
}
