% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entsoe_sftp.R
\name{entsoe_create_url_file}
\alias{entsoe_create_url_file}
\title{Retrieve a Specific File from ENTSO-E SFTP Server}
\usage{
entsoe_create_url_file(basis_name, year, month)
}
\arguments{
\item{basis_name}{A character string specifying the folder name within the ENTSO-E SFTP server's transparency platform.}

\item{year}{A numeric or character string specifying the year of the file to be retrieved.}

\item{month}{A numeric or character string specifying the month of the file to be retrieved.}
}
\value{
A response object containing the contents of the specified file.
}
\description{
This function requests and retrieves a specific CSV file from the ENTSO-E SFTP server based on
a folder (\code{basis_name}), year, and month. It authenticates using credentials stored in environment
variables (\code{ENTSOE_USER} and \code{ENTSOE_PASSWORD}).
}
\details{
The function constructs the file path from \code{basis_name}, \code{year}, and \code{month} and uses \code{curl}
to retrieve the specified CSV file from the server.
}
