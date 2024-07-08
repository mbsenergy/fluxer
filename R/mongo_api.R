#' Query MongoDB Data API
#'
#' This function queries a MongoDB collection using the MongoDB Data API.
#'
#' @param collection_name A character string specifying the name of the MongoDB collection. This parameter is required.
#' @param filter_list A named list of filters to apply to the query. Defaults to `NULL` to retrieve all documents.
#' @param sort_list A named list of sorting options. Defaults to `list("DATE" = -1)` to sort by "DATE" in descending order.
#' @param cluster A character string specifying the cluster name. Defaults to 'Cluster0'.
#' @param db_name A character string specifying the database name. Defaults to 'ECONOMICS'.
#' @param output A character string specifying the type of output: 'SIMPLE' for a data table or 'RAW' for the raw response. Defaults to 'SIMPLE'.
#' @param print_output A logical value indicating whether to print the output. Defaults to TRUE.
#' @return A data table containing the query results if `output` is 'SIMPLE', or the raw response if `output` is 'RAW'.
#' @import httr2 jsonlite data.table
#' @export
query_mongo = function(collection_name, filter_list = NULL, sort_list = list("DATE" = -1), cluster = 'Cluster0', db_name = 'ECONOMICS', output = 'SIMPLE', print_output = FALSE) {
    if (missing(collection_name) || !is.character(collection_name) || collection_name == "") {
        stop("The 'collection_name' parameter is required and must be a non-empty character string.")
    }

    # API Environment Variables
    url_base = Sys.getenv('MONGO_DATAAPI_URI')
    appId = Sys.getenv('MONGO_DATAAPI_APPID')
    apiVersion = Sys.getenv('MONGO_DATAAPI_VERSION')
    apiKey = Sys.getenv('MONGO_DATAAPI_KEY')

    if (url_base == "" || appId == "" || apiVersion == "" || apiKey == "") {
        stop("One or more MongoDB Data API environment variables are not set.")
    }

    url = paste0(url_base, '/app/', appId, '/endpoint/data/', apiVersion, '/action/find')

    # Build the request
    request = httr2::request(url) %>%
        httr2::req_headers(
            "apiKey" = apiKey,
            "Content-Type" = "application/json",
            "Accept" = "application/json"
        ) %>%
        httr2::req_body_json(list(
            dataSource = cluster,
            database = db_name,
            collection = collection_name,
            filter = filter_list,
            sort = sort_list
        ))

    # Perform the request
    response = httr2::req_perform(request)

    # Check for a successful request
    if (httr2::resp_status(response) == 200) {
        dtw = jsonlite::fromJSON(httr2::resp_body_string(response), flatten = TRUE)[[1]]
        data.table::setDT(dtw)

        # Transform date columns if they exist
        if ("c_date_update" %in% colnames(dtw)) {
            dtw[, c_date_update := data.table::as.IDate(c_date_update)]
        }

        if ("DATE" %in% colnames(dtw)) {
            dtw[, DATE := data.table::as.IDate(DATE)]
        }

    } else {
        message("Failed to fetch data. Status code: ", httr2::resp_status(response))
        message(httr2::resp_body_string(response))
        dtw = data.table::data.table()
    }

    if (output == 'SIMPLE') {
        if (isTRUE(print_output)) {
            print(dtw)
        }
        return(dtw)
    } else if (output == 'RAW') {
        return(response)
    } else {
        stop("Invalid value for 'output' parameter. Must be 'SIMPLE' or 'RAW'.")
    }
}



#' Generate MongoDB Projection String
#'
#' This function generates a MongoDB projection string from a vector of field names.
#'
#' @param v_field A character vector of field names to include in the projection.
#' @return A JSON string representing the MongoDB projection.
#' @export
fun_fields = function(v_field) {
    if (missing(v_field) || !is.character(v_field)) {
        stop("The 'v_field' parameter is required and must be a character vector.")
    }

    paste0('{"', paste(v_field, collapse = '": true, "'), '": true}')
}



#' Generate MongoDB Date Range Query
#'
#' This function generates a MongoDB query string for a date range.
#'
#' @param field_name The name of the date field to query.
#' @param start_date The start date of the range.
#' @param end_date The end date of the range.
#' @return A JSON string representing the MongoDB date range query.
#' @export
fun_query_date = function(field_name, start_date, end_date) {
    if (missing(field_name) || !is.character(field_name)) {
        stop("The 'field_name' parameter is required and must be a character string.")
    }

    start_date_tmp = as.POSIXct(as.Date(start_date), tz = "UTC")
    end_date_tmp = as.POSIXct(as.Date(end_date), tz = "UTC")

    paste(
        '"',
        field_name, '": {',
        '"$gte": { "$date": "', format(start_date_tmp, "%Y-%m-%dT%H:%M:%S.000Z"), '" }, ',
        '"$lte": { "$date": "', format(end_date_tmp, "%Y-%m-%dT%H:%M:%S.000Z"), '" }',
        '}',
        sep = ''
    )
}



#' Generate MongoDB Query for List of Values
#'
#' This function generates a MongoDB query string for a list of values.
#'
#' @param field_name The name of the field to query.
#' @param v_values A vector of values to include in the query.
#' @return A JSON string representing the MongoDB query for the list of values.
#' @export
fun_query_fl = function(field_name, v_values) {
    if (missing(field_name) || !is.character(field_name)) {
        stop("The 'field_name' parameter is required and must be a character string.")
    }

    if (missing(v_values) || !is.vector(v_values)) {
        stop("The 'v_values' parameter is required and must be a vector.")
    }

    paste0('"' ,field_name, '":{"$in": [', paste0('"', v_values, '"', collapse = ','), ']}')
}
