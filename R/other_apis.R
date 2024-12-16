#' Fetch Gas Data from AGSI API
#'
#' This function retrieves gas data for a specified country and date range using the AGSI API.
#' The results are returned as a data.table with detailed gas storage information.
#'
#' @param api_key Character. API key for authenticating with the AGSI API.
#' @param country Character. Country code for which the data is to be fetched (e.g., "IT").
#' @param from_date Character. Start date for the data retrieval in "YYYY-MM-DD" format.
#' @param to_date Character. End date for the data retrieval in "YYYY-MM-DD" format.
#' 
#' @return A data.table containing gas data for the specified country and date range.
#' If the API request fails, an empty data.table is returned.
#'
#' @details 
#' The function fetches data from the AGSI API, iterating over multiple pages if necessary. 
#' It consolidates the results into a single data.table for further analysis.
#'
#' @examples
#' \dontrun{
#' api_key <- 'your_api_key_here'
#' country <- "IT"
#' from_date <- "2023-10-01"
#' to_date <- "2023-12-01"
#' data <- fetch_country_data(api_key, country, from_date, to_date)
#' }
#' 
#' @import httr jsonlite data.table tidyjson
#' @export
agsi_gas_data <- function(country, from_date, to_date, api_key = Sys.getenv('AGSI_KEY')) {

  # Initialize an empty data.table to store results
  dtw <- data.table()

  # Build the base URL
  url_s <- sprintf("https://agsi.gie.eu/api?&country=%s&size=300&from=%s&to=%s&reverse=false", 
                   country, from_date, to_date)

  # Make the initial API request
  res <- GET(url_s, add_headers(`x-key` = api_key))

  if (status_code(res) != 200) {
    warning(sprintf("Failed to fetch data for country %s", country))
    return(dtw)
  }

  ruby <- fromJSON(content(res, as = "text", encoding = 'UTF-8'))
  last_page <- ruby$last_page

  # Loop through pages from last to first
  for (i in seq(last_page, 1)) {
    url_s_page <- paste0(url_s, "&page=", i)
    res <- GET(url_s_page, add_headers(`x-key` = api_key))

    if (status_code(res) != 200) {
      warning(sprintf("Failed to fetch data for country %s on page %d", country, i))
      next
    }

    ruby <- fromJSON(content(res, as = "text", encoding = 'UTF-8'))$data
    dts <- tidyjson::as_tibble(ruby)
    setDT(dts)

    dtw <- rbind(dtw, dts, fill = TRUE)
  }
  dtw[, info := NULL]
  dtw[, url := NULL]
  dtw[, updatedAt := NULL]
  dtw[, name := NULL]
  dtw[, DATE := gasDayStart]
  setnames(dtw, 'code', 'CODE_2')
  dts = melt(dtw, id.vars = c('CODE_2', 'DATE'), variable.name = 'VARIABLE', variable.factor = FALSE, value.name = 'VALUE')
  return(dts)
}
