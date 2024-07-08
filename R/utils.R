#' Get the Latest Date from a MongoDB Collection
#'
#' This function retrieves the latest record by date from a MongoDB collection.
#'
#' @param collection An object representing the MongoDB collection connection.
#' @return A list containing the latest date (`DATE`) and the associated value (`VALUE`), or `NULL` if the collection is empty.
#' @import data.table
#' @export
get_latest_date = function(collection) {
    if (missing(collection) || !inherits(collection, "mongolite::mongo")) {
        stop("The 'collection' parameter is required and must be a 'mongolite::mongo' object.")
    }

    # Find the latest record by date
    latest_record = collection$find(sort = '{"DATE": -1}', limit = 1)

    # Check if the collection is empty
    if (nrow(latest_record) == 0) {
        return(NULL)
    } else {
        return(list(DATE = data.table::as.IDate(latest_record$DATE), VALUE = as.numeric(latest_record$VALUE)))
    }
}
