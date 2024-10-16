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
#' @param apikey Optional character. MotherDuck API token. If not provided, the
#'   function will use the token from the environment variable `MOTHERDUCK`.
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
#' @import DBI
#' @import duckdb
#' @import glue
#' @import crayon
#' @importFrom DBI dbConnect dbExecute dbIsValid
#' @importFrom glue glue_sql
#' @importFrom crayon bgGreen bgRed

connect_md <- function(store_conn = TRUE, apikey = FALSE) {
    # Create a DuckDB connection to local.duckdb
    conn <- DBI::dbConnect(duckdb::duckdb(), "local.duckdb")

    # Install and load MotherDuck extension
    DBI::dbExecute(conn, "INSTALL 'motherduck';")
    DBI::dbExecute(conn, "LOAD 'motherduck';")

    # Check if apikey is provided, otherwise fallback to environment variable
    if (is.character(apikey)) {
        auth_token <- apikey
    } else {
        auth_token <- Sys.getenv('MOTHERDUCK')
    }

    # Authenticate with MotherDuck
    auth_query <- glue::glue_sql("SET motherduck_token = {`auth_token`};", .con = conn)
    DBI::dbExecute(conn, auth_query)

    # Control variable to check if connection was successful
    connection_successful <- FALSE

    # Try connecting to MotherDuck
    tryCatch({
        DBI::dbExecute(conn, "PRAGMA MD_CONNECT")
        connection_successful <- TRUE  # Set to TRUE only if PRAGMA MD_CONNECT succeeds
    }, error = function(e) {
        if (grepl("Request failed: Your request is not authenticated. Please check your MotherDuck token.", e$message)) {
            message(glue::glue("{crayon::bgRed('[ERROR]')} MotherDuck API TOKEN is MISSING or INCORRECT."))
        } else {
            stop(e)
        }
    })

    # If connection was successful, check if conn is valid and store in global env if needed
    if (connection_successful) {
        if (isTRUE(DBI::dbIsValid(conn))) {
            message(glue::glue("{crayon::bgGreen('[OK]')} Connected to Flux MotherDuck."))
        } else {
            message(glue::glue("{crayon::bgRed('[ERROR]')} Could NOT connect to Flux MotherDuck."))
        }

        # Store the connection in the global environment if requested
        if (isTRUE(store_conn)) {
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
#' @import DBI
#' @import glue
#' @import crayon
#' @import data.table
#' @importFrom DBI dbGetQuery dbIsValid
#' @importFrom glue glue
#' @importFrom crayon bgGreen bgYellow bgRed red
#' @importFrom data.table as.data.table

check_database_md = function(connection, database_name) {

    conn = connection

    if (isTRUE(DBI::dbIsValid(conn))) {
        md_databases = DBI::dbGetQuery(conn, 'select * from MD_ALL_DATABASES();') %>% as.data.table()
        md_databases = md_databases[type == 'motherduck' & is_attached == TRUE]$alias
        if (database_name %in% md_databases) {
            message(glue::glue("{crayon::bgGreen('[OK]')} Database '{crayon::bgGreen(database_name)}' exists."))
            return(TRUE)
        } else {
            message(glue::glue("{crayon::bgYellow('[MISSING]')} Database '{crayon::bgYellow(database_name)}' does NOT exist."))
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
#' @import DBI
#' @import glue
#' @import crayon
#' @importFrom DBI dbListTables dbIsValid
#' @importFrom glue glue
#' @importFrom crayon bgGreen bgYellow bgRed red

check_table_md = function(connection, table_name) {

    conn = connection

    if (isTRUE(DBI::dbIsValid(conn))) {
        md_tables = DBI::dbListTables(conn)
        if (table_name %in% md_tables) {
            message(glue::glue("{crayon::bgGreen('[OK]')} Table '{crayon::bgGreen(table_name)}' exists."))
            return(TRUE)
        } else {
            message(glue::glue("{crayon::bgYellow('[MISSING]')} Table '{crayon::bgYellow(table_name)}' does NOT exist."))
            return(FALSE)
        }
    } else {
        message(crayon::red("{crayon::bgRed('[ERROR]')} Connection is invalid or closed."))
        return(FALSE)
    }
}



#' Create an Empty Table in MotherDuck Database Dynamically
#'
#' This function creates a new table in a specified MotherDuck database
#' based on the column names and types of the input data.
#'
#' @param conn A valid DuckDB connection object.
#' @param database_name The name of the MotherDuck database where the table will be created.
#' @param table_name A character string representing the name of the new table.
#' @param data A data frame or data table (`dt_all[[i]]`) from which column names and types are derived.
#'
#' @return TRUE if the table is successfully created, FALSE otherwise.
#' @examples
#' \dontrun{
#'   create_empty_table_md(conn, "economics_db", "new_table", mtcars)
#' }
#' @export
create_empty_table_md <- function(conn, database_name, table_name, data) {

    # Check if the connection is valid
    if (!isTRUE(DBI::dbIsValid(conn))) {
        stop("Connection is invalid or closed.")
    }

    # Ensure the table name and database name are not empty
    if (missing(database_name) || missing(table_name) || missing(data)) {
        stop("Database name, table name, and data must be provided.")
    }

    # Generate the column names and their SQL data types based on the input data
    cols <- colnames(data)
    types <- sapply(data, function(col) {
        if (is.integer(col)) {
            return("INTEGER")
        } else if (is.numeric(col)) {
            return("DOUBLE")
        } else if (is.character(col)) {
            return("VARCHAR")
        } else if (is.logical(col)) {
            return("BOOLEAN")
        } else {
            stop("Unsupported data type detected.")
        }
    })

    # Combine column names and their types into a SQL string
    col_definitions <- paste(cols, types, collapse = ", ")

    # Build the full SQL CREATE TABLE statement
    create_table_query <- glue::glue_sql(
        "CREATE TABLE {`database_name`}.{`table_name`} ({col_definitions});",
        .con = conn
    )

    # Execute the SQL statement to create the table
    tryCatch({
        DBI::dbExecute(conn, create_table_query)
        message(glue::glue("{crayon::bgGreen('[OK]')} Table '{table_name}' created successfully in '{database_name}'."))
        return(TRUE)
    }, error = function(e) {
        message(glue::glue("{crayon::bgRed('[ERROR]')} Failed to create table '{table_name}' in '{database_name}': {e$message}"))
        return(FALSE)
    })
}


#' Retrieve the Last Date from a Table in a Database
#'
#' This function retrieves the most recent date from a specified table in a database.
#' It executes a SQL query that orders the data by the date column in descending order
#' and limits the results to one record. This is useful for determining the latest
#' entry in time series data or for managing data updates.
#'
#' @param con A DBI connection object to the database from which to retrieve data.
#' @param database_name A string representing the name of the database.
#' @param table_name A string representing the name of the table from which to retrieve the date.
#' @param verbose A logical value indicating whether to print the result of the query.
#'        Defaults to `FALSE`.
#'
#' @return A data frame containing the most recent row of data from the specified table,
#'         or `NULL` if the query fails.
#'
#' @details
#' - The function constructs a SQL `SELECT` statement to fetch all columns from the specified
#'   table, ordering the results by the `DATE` column in descending order.
#' - If the query is successful, the result is printed if `verbose` is set to `TRUE`,
#'   and the result is returned as a data frame.
#' - If an error occurs during the execution of the query, an error message is displayed,
#'   and the function returns `NULL`.
#'
#' @examples
#' \dontrun{
#'   # Assuming 'con' is a valid DBI connection
#'   last_date <- get_last_date_sql(con, "my_database", "my_table", verbose = TRUE)
#'   print(last_date)
#' }
#'
#' @export
#' @import DBI
#' @import glue
#' @import crayon
#' @importFrom DBI dbGetQuery
#' @importFrom glue glue_sql
#' @importFrom crayon bgRed

get_last_date_sql = function (con, database_name, table_name, verbose = FALSE) {

    # Ensure the connection is valid
    if (!isTRUE(DBI::dbIsValid(con))) {
        stop("The connection is invalid or closed.")
    }

    # Create the query
    query <- glue::glue_sql("SELECT * FROM {`database_name`}.{`table_name`} ORDER BY DATE DESC LIMIT 1;",
                            .con = con)

    # Execute the query with error handling
    tryCatch({
        result <- DBI::dbGetQuery(con, query)

        # Print the result if verbose is TRUE
        if (verbose) {
            print(result)
        }
        return(result)

    }, error = function(e) {
        message(glue::glue("{crayon::bgRed('[ERROR]')} Failed to retrieve data from '{table_name}': {e$message}"))
        return(NULL)
    })
}






