#' @title
#' Retrieve Day-Ahead Market Prices for a Specific Country
#'
#' @description
#' This function retrieves Day-Ahead Market (DAM) prices for a specific country from the ENTSOE Transparency Platform API.
#' The function fetches market price data for a given time range and country, processes the data, and formats it into a `data.table`.
#'
#' @param country A character string specifying the country for which to retrieve DAM prices. The country should match an entry in the `entsoe_countries` table.
#' @param from_data A Date object specifying the start date for the data range. Default is 365 days before the current date.
#' @param to_data A Date object specifying the end date for the data range. Default is the current date.
#' @param api_key A character string containing the ENTSOE API key. It can be passed directly or retrieved from the environment variable `ENTSOE_KEY`.
#'
#' @return
#' A `data.table` containing the formatted DAM price data for the specified country and time range. The data table includes:
#' \itemize{
#'   \item `DATE`: The date in `YYYY-MM-DD` format.
#'   \item `HOUR`: The hour of the day (position in the data).
#'   \item `VALUE`: The price value in EUR.
#'   \item `UNIT`: The unit of the price (always 'EUR').
#' }
#'
#' @import data.table
#' @import magrittr
#' @import xml2
#' @import httr
#' @import glue
#' @import crayon
#'
#' @examples
#' # Example usage to retrieve DAM prices for Italy (North)
#' country <- "Italy (North)"
#' from_data <- Sys.Date() - 365
#' to_data <- Sys.Date()
#' dam_prices <- entsoe_dam_prices(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY'))
#' print(dam_prices)
#'
#' @export
entsoe_dam_prices = function(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY')) {

  # Translate the country to the EIC code using the entsoe_countries mapping
  entsoe_domain = entsoe_countries[COUNTRY == country]$EIC_CODE

  # Format the start and end period times for the API request
  period_start <- paste0(format(as.Date(from_data), "%Y%m%d"), "0000")
  period_end <- paste0(format(as.Date(to_data), "%Y%m%d"), "2300")

  # Construct the full URL with query parameters
  base_url <- "https://web-api.tp.entsoe.eu/api"

  url <- paste0(
    base_url,
    "?securityToken=", api_key,
    "&documentType=", 'A44',
    "&out_Domain=", entsoe_domain,
    "&in_Domain=", entsoe_domain,
    "&periodStart=", period_start,
    "&periodEnd=", period_end
  )

  # Make the GET request to the ENTSOE API
  res <- VERB("GET", url = url, httr::content_type_xml(), httr::write_memory())

  # Check for successful response
  if (status_code(res) == 200) {
    # Return the content of the response if successful
    res_content = content(res, encoding = 'UTF-8')
  } else {
    # Return an error message if the request fails
    stop(paste("API call failed with status code:", status_code(res)))
  }

  # Parse the XML response and extract the data using the extract_xml_data function
  result <- extract_xml_data(res_content)

  # Process the extracted data and format it
  DT = result[, .(COUNTRY = country, DATE = format(suppressWarnings(strptime(StartTime, "%Y-%m-%dT%H:%MZ")), "%Y-%m-%d"),
                  HOUR = Position, VALUE = Price, UNIT = 'EUR')]

  # Display a success message with formatting using glue and crayon
  message <- glue("[{crayon::bold('OK')}] DATA for {crayon::bold(country)} from {crayon::bold(from_data)} to {crayon::bold(to_data)} retrieved correctly")
  cat(crayon::green(message), "\n")

  # Return the extracted and formatted data as a data.table
  return(DT)
}
