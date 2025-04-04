% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gme_download.R
\name{gme_get_directory}
\alias{gme_get_directory}
\title{Retrieve FTP Directory Listing from GME}
\usage{
gme_get_directory(username = "PIASARACENO", password = "18N15C9R")
}
\arguments{
\item{username}{FTP username. Default is \code{"PIASARACENO"}.}

\item{password}{FTP password. Default is \code{"18N15C9R"}.}
}
\value{
A \code{data.table} containing the directory listing.
}
\description{
This function connects to the GME FTP server and retrieves a directory listing.
}
\examples{
\dontrun{
file_dt = gme_get_directory()
print(file_dt)
}
}
