% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gme_download.R
\name{gme_validate_filename}
\alias{gme_validate_filename}
\title{Validate Filename Structure}
\usage{
gme_validate_filename(filename, num_digits = 8, file_extension = "xml")
}
\arguments{
\item{filename}{A string representing the filename to validate.}

\item{num_digits}{An integer specifying the number of leading digits in the filename. Default is 8.}

\item{file_extension}{A string specifying the expected file extension (without the dot). Default is "xml".}

\item{middle_expression}{A string specifying the exact middle expression in the filename. Default is "MGPPrezzi".}
}
\value{
A logical value (\code{TRUE} if the filename is valid, \code{FALSE} otherwise).
If the filename is invalid, it prints "Wrong filename" to the console.
}
\description{
This function checks whether a given filename matches a specific structure based on
the number of leading digits, a middle expression, and the file extension.
}
\examples{
# Validate a correctly formatted filename
gme_validate_filename("20241126MGPPrezzi.xml") # Returns TRUE

# Validate an incorrectly formatted filename
gme_validate_filename("wrongfile.xml") # Prints "Wrong filename" and returns FALSE

# Validate with custom parameters
gme_validate_filename("2024MGPPrezzi.txt", num_digits = 4, file_extension = "txt") # Returns TRUE

}
