% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_utils.R
\name{table_obs_dist}
\alias{table_obs_dist}
\title{Count unique values in a column of a PostgreSQL table}
\usage{
table_obs_dist(con, table_name, column_name)
}
\arguments{
\item{con}{A \code{DBI} connection object to the PostgreSQL database.}

\item{table_name}{A character string specifying the name of the table.}

\item{column_name}{A character string specifying the column to analyze.}
}
\value{
A data frame with unique values of the column and their counts.
}
\description{
This function connects to a PostgreSQL database, retrieves the distinct values
of a specified column in a table, and counts their occurrences.
}
