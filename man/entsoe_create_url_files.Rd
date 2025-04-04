% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entsoe_sftp.R
\name{entsoe_create_url_files}
\alias{entsoe_create_url_files}
\title{Retrieve Folder Contents from ENTSO-E SFTP Server}
\usage{
entsoe_create_url_files(basis_name)
}
\arguments{
\item{basis_name}{A character string specifying the name of the folder within the ENTSO-E SFTP server's transparency platform.}
}
\value{
A response object containing the contents of the specified folder on the SFTP server.
}
\description{
This function requests and retrieves the contents of a specified folder on the ENTSO-E SFTP server,
given a folder name (\code{basis_name}). It authenticates using credentials stored in environment
variables (\code{ENTSOE_USER} and \code{ENTSOE_PASSWORD}).
}
\details{
This function utilizes \code{curl} to securely access the specified folder via SFTP.
Ensure the environment variables \code{ENTSOE_USER} and \code{ENTSOE_PASSWORD} contain valid credentials.
}
