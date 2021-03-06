% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read a .csv file into a tibble}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A character string representing a path to a file.}
}
\value{
This function returns a tibble read from the requested file.
}
\description{
This is a function to read a .csv file with the given file name into a tibble.
Conditions that may result in an error: \code{filename} is not a valid path.
}
\examples{
\dontrun{fars_read("accident_2013.csv.bz2")}

}
