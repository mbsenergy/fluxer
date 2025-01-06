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

  message(crayon::bgCyan(paste('DATES:', result)))
  
  # Convert the result to a data.table
  result_dt <- as.data.table(result)
  
  # Return the data.table
  return(result_dt)
}