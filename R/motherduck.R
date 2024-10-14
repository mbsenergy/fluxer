#' Connect to a MotherDuck Database
#'
#' This function establishes a connection to a local DuckDB database and connects
#' to the MotherDuck API using the authentication token stored in the environment
#' variable `MOTHERDUCK`. It optionally stores the connection in the global
#' environment for future use.
#'
#' The function also handles authentication errors and informs the user if the
#' connection was successful or not.
#'
#' @param store_conn Logical, default is `TRUE`. If `TRUE`, the function stores the
#'   established DuckDB connection object (`conn`) in the global environment. If
#'   `FALSE`, it does not store the connection.
#'
#' @details
#' The function first establishes a local DuckDB connection and installs and loads
#' the `motherduck` extension. It then sets the MotherDuck token using the token
#' stored in the `MOTHERDUCK` environment variable. If the connection is successful,
#' it confirms the connection status and optionally stores the connection.
#'
#' - If the MotherDuck token is missing or invalid, the function returns an error message.
#' - If the connection is successful, a message indicating success is displayed.
#'
#' @return
#' The function returns no value but will optionally store the `conn` object
#' in the global environment if `store_conn = TRUE`. On failure, error messages
#' are printed, and the function will stop execution if the error is not related
#' to authentication.
#'
#' @examples
#' \dontrun{
#'   # Connect and store the connection globally
#'   connect_md(TRUE)
#'
#'   # Connect without storing the connection globally
#'   connect_md(FALSE)
#' }
#'
#' @export

connect_md = function(store_conn = TRUE) {

    conn = DBI::dbConnect(duckdb::duckdb(), "local.duckdb")
    DBI::dbExecute(conn, "INSTALL 'motherduck';")
    DBI::dbExecute(conn, "LOAD 'motherduck';")

    # Authenticate with MotherDuck
    auth_query = glue::glue_sql("SET motherduck_token= {`Sys.getenv('MOTHERDUCK')`};", .con = conn)
    DBI::dbExecute(conn, auth_query)

    # Control variable to check success
    connection_successful = FALSE

    # Connect to MotherDuck
    tryCatch({
        DBI::dbExecute(conn, "PRAGMA MD_CONNECT")
        connection_successful = TRUE  # Set to TRUE only if PRAGMA MD_CONNECT succeeds
    }, error = function(e) {
        if (grepl("Request failed: Your request is not authenticated. Please check your MotherDuck token.", e$message)) {
            message(glue("{crayon::bgRed('[ERROR]')} Motherduck API TOKEN is MISSING or INCORRECT."))
        } else {
            stop(e)
        }
    })

    # Continue only if connection was successful
    if (connection_successful) {
        if(isTRUE(DBI::dbIsValid(conn))) {
            message(glue("{crayon::bgGreen('[OK]')} Connected to Flux Motherduck."))
        } else {
            message(glue("{crayon::bgRed('[ERROR]')} Could NOT connect to Flux Motherduck."))
        }

        if(isTRUE(store_conn)) {
            assign('conn', conn, envir = .GlobalEnv)
        }
    }

}


#' Check if a MotherDuck Database Exists
#'
#' This function checks whether a specified database exists in the connected
#' MotherDuck environment using an active DuckDB connection.
#'
#' @param connection A valid DuckDB connection object, typically returned by
#'   `DBI::dbConnect()`. This connection must be established and remain active.
#' @param database_name A character string representing the alias of the
#'   database you want to check.
#'
#' @details
#' The function queries the list of all available databases using the
#' `MD_ALL_DATABASES()` function in MotherDuck, then filters the results for
#' active, attached databases of type 'motherduck'. If the `database_name` exists
#' in the list, the function returns `TRUE` and prints a success message. Otherwise,
#' it returns `FALSE` and notifies the user that the database does not exist.
#'
#' If the connection object is invalid or closed, the function will return
#' `FALSE` and print an error message.
#'
#' @return
#' A logical value:
#' - `TRUE` if the specified `database_name` exists in the MotherDuck databases.
#' - `FALSE` if the database does not exist or the connection is invalid.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have an active connection named 'conn'
#'   check_database_md(conn, "my_database")
#' }
#'
#' @export
check_database_md = function(connection, database_name) {

    conn = connection

    if (isTRUE(DBI::dbIsValid(conn))) {
        md_databases = DBI::dbGetQuery(conn, 'select * from MD_ALL_DATABASES();') %>% as.data.table()
        md_databases = md_databases[type == 'motherduck' & is_attached == TRUE]$alias
        if (database_name %in% md_databases) {
            message(glue("{crayon::bgGreen('[OK]')} Database '{crayon::bgGreen(database_name)}' exists."))
            return(TRUE)
        } else {
            message(glue("{crayon::bgYellow('[MISSING]')} Database '{crayon::bgYellow(database_name)}'does NOT exist."))
            return(FALSE)
        }
    } else {
        message(crayon::red("{crayon::bgRed('[ERROR]')} Connection is invalid or closed."))
        return(FALSE)
    }

}


#' Check if a Table Exists in a MotherDuck Database
#'
#' This function checks whether a specified table exists in the connected
#' MotherDuck environment using an active DuckDB connection.
#'
#' @param connection A valid DuckDB connection object, typically returned by
#'   `DBI::dbConnect()`. This connection must be established and remain active.
#' @param table_name A character string representing the name of the table you want to check.
#'
#' @details
#' The function retrieves all tables in the currently connected database using
#' `DBI::dbListTables()`. If the specified `table_name` exists in the list of
#' tables, the function returns `TRUE` and prints a success message. Otherwise,
#' it returns `FALSE` and notifies the user that the table does not exist.
#'
#' If the connection object is invalid or closed, the function will return
#' `FALSE` and print an error message.
#'
#' @return
#' A logical value:
#' - `TRUE` if the specified `table_name` exists in the connected database.
#' - `FALSE` if the table does not exist or the connection is invalid.
#'
#' @examples
#' \dontrun{
#'   # Assuming you have an active connection named 'conn'
#'   check_table_md(conn, "my_table")
#' }
#'
#' @export
check_table_md = function(connection, table_name) {

    conn = connection

    if (isTRUE(DBI::dbIsValid(conn))) {
        md_tables = DBI::dbListTables(conn)
        if (table_name %in% md_tables) {
            message(glue("{crayon::bgGreen('[OK]')} Table '{crayon::bgGreen(table_name)}' exists."))
            return(TRUE)
        } else {
            message(glue("{crayon::bgYellow('[MISSING]')} Table '{crayon::bgYellow(table_name)}' does NOT exist."))
            return(FALSE)
        }
    } else {
        message(crayon::red("{crayon::bgRed('[ERROR]')} Connection is invalid or closed."))
        return(FALSE)
    }

}


