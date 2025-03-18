#' Get Minimum and Maximum Dates from a Table
#'
#' This function queries a PostgreSQL database table to retrieve the minimum and maximum dates
#' from a specified date column.
#'
#' @param con A database connection object created using \code{DBI::dbConnect()}.
#' @param table_name A character string specifying the name of the table to query.
#' @param date_column A character string specifying the name of the date column. Defaults to \code{"DATE"}.
#'
#' @return A \code{data.table} with two columns:
#'   \item{min_date}{The earliest date in the specified column.}
#'   \item{max_date}{The latest date in the specified column.}
#'
#' @examples
#' \dontrun{
#' # Establish a connection to PostgreSQL
#' con <- DBI::dbConnect(
#'   RPostgres::Postgres(),
#'   dbname = "your_database",
#'   host = "your_host",
#'   port = 5432,
#'   user = "your_user",
#'   password = "your_password"
#' )
#'
#' # Get the minimum and maximum dates from the table
#' result_dt <- db_get_minmax_dates(con, table_name = "TS01_entsoe_dam_prices", date_column = "DATE")
#' print(result_dt)
#'
#' # Disconnect from the database
#' DBI::dbDisconnect(con)
#' }
#'
#' @export
db_get_minmax_dates <- function(con, table_name, date_column = "DATE") {
  # Validate inputs
  if (missing(con) || missing(table_name)) {
    stop("Both 'con' (database connection) and 'table_name' are required.")
  }

  # Construct the SQL query using glue
  query <- glue('SELECT MIN("{date_column}") AS min_date, MAX("{date_column}") AS max_date FROM "{table_name}";')

  # Execute the query
  result <- dbGetQuery(con, query)

  # Convert the result to a data.table
  result_dt <- as.data.table(result)

  message(crayon::bgCyan(paste('FROM:', result_dt$min_date)))
  message(crayon::bgCyan(paste('TO:', result_dt$max_date)))

  # Return the data.table
  return(result_dt)
}



#' Find Missing Dates in a Vector
#'
#' This function identifies the missing dates in a sequence of dates. It can optionally exclude weekends (Saturday and Sunday) from the results.
#'
#' @param dates A character vector of dates in "YYYY-MM-DD" format.
#' @param full_week Logical. If `TRUE`, includes all missing dates (default). If `FALSE`, only includes missing weekdays (Monday to Friday).
#' @return A character vector of missing dates in "YYYY-MM-DD" format.
#' @examples
#' # Example input vector
#' ret_files <- c(
#'   "2023-12-31", "2024-12-01", "2024-12-02", "2024-12-03", "2024-12-04",
#'   "2024-12-14", "2024-12-20", "2024-12-21", "2024-12-22", "2024-12-23",
#'   "2024-12-24", "2024-12-25", "2024-12-26", "2024-12-27", "2024-12-28",
#'   "2024-12-29"
#' )
#'
#' # Find all missing dates (full week)
#' find_missing_dates(ret_files, full_week = TRUE)
#'
#' # Find only missing weekdays (Monday to Friday)
#' find_missing_dates(ret_files, full_week = FALSE)
#'
#' @export
find_missing_dates = function(dates, full_week = TRUE) {
  # Convert the input vector to Date objects
  date_vector = as.Date(dates)

  # Generate a full sequence of dates from the minimum to the maximum
  full_date_range = seq(min(date_vector), max(date_vector), by = "day")

  # Filter missing dates
  missing_dates = full_date_range[!full_date_range %in% date_vector]

  # If full_week is FALSE, filter out weekends (Saturday and Sunday)
  if (!full_week) {
    missing_dates = missing_dates[!weekdays(missing_dates) %in% c("Saturday", "Sunday")]
  }

  # Return the missing dates as a character vector
  return(as.character(missing_dates))
}


#' Convert Dates to First Day of the Month
#'
#' This function transforms a given date (or vector of dates) into the first day of the same month, formatted as `YYYY-MM-01`.
#'
#' @param dates A vector of dates in `"YYYY-MM-DD"` format. Can be `character`, `Date`, or `IDate`.
#' @return A vector of dates converted to the first day of the respective month (`IDate` format).
#' @export
to_yymm = function(dates) {
  dates = as.IDate(dates)  # Ensure it's a date
  return(as.IDate(paste0(format(dates, "%Y-%m"), "-01")))
}


#' Count unique values in a column of a PostgreSQL table
#'
#' This function connects to a PostgreSQL database, retrieves the distinct values
#' of a specified column in a table, and counts their occurrences.
#'
#' @param con A `DBI` connection object to the PostgreSQL database.
#' @param table_name A character string specifying the name of the table.
#' @param column_name A character string specifying the column to analyze.
#'
#' @return A data frame with unique values of the column and their counts.
#' @export
#'
table_obs_dist <- function(con, table_name, column_name) {
  # Construct the SQL query dynamically
  query <- sprintf(
    'SELECT "%s", COUNT(*) AS count
     FROM "%s"
     GROUP BY "%s"
     ORDER BY "%s";',
    column_name, table_name, column_name, column_name
  )

  # Execute the query and return the result
  result <- DBI::dbGetQuery(con, query)

  return(result)
}


library(data.table)

#' Count unique values in a column of a data.table
#'
#' This function takes a data.table, groups by the specified column,
#' and counts occurrences of each unique value.
#'
#' @param dt A `data.table` object.
#' @param column_name A character string specifying the column to analyze.
#'
#' @return A `data.table` with unique values of the column and their counts.
#' @export
#'
#' @examples
#' dt <- data.table(DATE = c("2024-01-01", "2024-01-02", "2024-01-01", "2024-01-03"))
#' count_column_values(dt, "DATE")
dt_obs_dist <- function(dt, column_name) {
  # Ensure input is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table")
  }

  # Perform the count operation
  result <- dt[, .N, by = .(get(column_name))] # Correct column referencing

  return(result)
}

#' Compare Column Names Between Two data.tables
#'
#' This function compares the column names of two `data.table` objects and identifies which columns are missing in each.
#'
#' @param dt1 A `data.table` object.
#' @param dt2 A `data.table` object.
#' @param return_list Logical. If `TRUE`, the function returns a list with missing columns; if `FALSE`, it returns a formatted `kable` table. Default is `FALSE`.
#'
#' @return If `return_list = TRUE`, a named list with:
#'   \item{missing_in_dt1}{Columns present in `dt2` but missing in `dt1`}
#'   \item{missing_in_dt2}{Columns present in `dt1` but missing in `dt2`}
#' If `return_list = FALSE`, a `kable` table showing missing columns.
#'
#' @examples
#' library(data.table)
#'
#' dt1 = data.table(a = 1:5, b = 6:10, c = 11:15)
#' dt2 = data.table(b = 1:5, c = 6:10, d = 11:15)
#'
#' # Return a kable table
#' compare_colnames(dt1, dt2)
#'
#' # Return a list with missing columns
#' compare_colnames(dt1, dt2, return_list = TRUE)
#'
#' @import data.table knitr
#' @export
compare_colnames = function(dt1, dt2, return_list=FALSE) {
    dt1_name = deparse(substitute(dt1))
    dt2_name = deparse(substitute(dt2))

    cols1 = colnames(dt1)
    cols2 = colnames(dt2)

    missing_in_dt1 = setdiff(cols2, cols1) # Columns in dt2 but not in dt1
    missing_in_dt2 = setdiff(cols1, cols2) # Columns in dt1 but not in dt2

    if (return_list) {
        return(list(
            setNames(list(missing_in_dt1), paste0("missing_in_", dt1_name)),
            setNames(list(missing_in_dt2), paste0("missing_in_", dt2_name))
        ))
    } else {
        result_dt = data.table::data.table(
            Column = c(missing_in_dt1, missing_in_dt2),
            Missing_In = c(rep(dt1_name, length(missing_in_dt1)), rep(dt2_name, length(missing_in_dt2)))
        )

        return(knitr::kable(result_dt, format = "markdown"))
    }
}



