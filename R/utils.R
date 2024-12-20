#' Make an API Request to ENTSO-E Web API
#'
#' This function sends a request to the ENTSO-E (European Network of Transmission System Operators for Electricity) Web API, retrieves data based on the given query, and processes the response. It handles both XML and zip file responses.
#'
#' @param api_scheme A string representing the API scheme, default is `"https://"`. This is the protocol used for the API request.
#' @param api_domain A string representing the domain name of the API, default is `"web-api.tp.entsoe.eu/"`.
#' @param api_name A string representing the API name or endpoint, default is `"api?"`.
#' @param query_string A string representing the query parameters to be appended to the API URL. This parameter is required.
#' @param api_key A string representing the security token used for authentication. This parameter is required.
#'
#' @return A list containing the response data:
#' \describe{
#'   \item{en_cont_list}{A list of XML content if the response is a zip file. The list contains the decompressed XML files.}
#'   \item{en_cont}{XML content if the response is an XML document.}
#' }
#'
#' @details
#' The function first validates the presence of the required arguments: `query_string` and `api_key`. If the API request is successful, it handles both XML responses and zip file responses:
#' - If the response is a zip file (`application/zip`), it downloads and extracts the XML content.
#' - If the response is XML (`text/xml` or `application/xml`), it returns the XML content directly.
#'
#' If the request fails with a `200 OK` response but contains an error message or exceeds the data limit, the function checks for the need to offset the query and recursively retries with a new query offset.
#'
#' @examples
#' \dontrun{
#' # Example usage of the api_req function
#' query_string <- "documentType=A63&businessType=A85"
#' api_key <- "your_api_key"
#' result <- api_req(query_string = query_string, api_key = api_key)
#' print(result)
#' }
#'
#' @import httr
#' @import xml2
#' @import purrr
#' @import stringr
#' @export
api_req <- function(
    api_scheme = "https://",
    api_domain = "web-api.tp.entsoe.eu/",
    api_name = "api?",
    query_string = NULL,
    api_key = NULL
) {
  if (is.null(query_string)) {
    stop("The argument 'query_string' is missing!")
  }
  if (is.null(api_key)) {
    stop("The argument 'api_key' is not provided!")
  } else {
    # add the canonical API prefix and suffix to the request url
    url <- paste0(
      api_scheme, api_domain, api_name, query_string, "&securityToken="
    )
    message(url, "<...>")
  }

  resp <- httr::GET(
    url = paste0(url, api_key),
    httr::content_type_xml(),
    httr::write_memory()
  )

  if (is.integer(httr::status_code(resp))) message("response has arrived")

  # if the get request is successful, then ...
  if (httr::status_code(resp) == "200") {

    # if the request is a zip file, then ...
    rhct <- resp$headers$`content-type`
    if (rhct == "application/zip") {

      # redownload again, but into disk this time
      temp_file_path <- tempfile(fileext = ".zip")
      resp <- httr::GET(
        url = paste0(url, api_key),
        httr::content_type_xml(),
        httr::write_disk(path = temp_file_path, overwrite = TRUE)
      )

      # read the xml content from each the decompressed files
      en_cont_list <- read_zipped_xml(temp_file_path)

      # return with the xml content list
      return(en_cont_list)

    } else if (rhct %in% c("text/xml", "application/xml")) {

      # read the xml content from the response
      en_cont <- httr::content(resp, encoding = "UTF-8")

      # return with the xml content
      return(en_cont)

    } else {

      stop("Not known response content-type: ", resp$headers$`content-type`)

    }

  } else {

    # extract reason from reason text
    response_reason <- resp |>
      httr::content(encoding = "utf-8") |>
      xml2::as_list() |>
      purrr::pluck("Acknowledgement_MarketDocument", "Reason", "text") |>
      unlist()
    if (lengths(response_reason) > 0) {
      message("*** ", response_reason, " ***")
    }

    # check if offset usage needed or not
    offset_needed <- stringr::str_detect(
      string = response_reason,
      pattern = "The amount of requested data exceeds allowed limit"
    )

    # check if query offset is allowed
    offset_allowed <- stringr::str_detect(
      string = query_string,
      pattern = "documentType=A63&businessType=A85",
      negate = TRUE
    )

    # if offset usage needed, then ...
    if (isTRUE(offset_needed) && isTRUE(offset_allowed)) {

      # calculate offset URLs
      offset_query_strings <- calc_offset_urls(
        reason = response_reason,
        query_string = query_string
      )

      # recursively call the api_req() function itself
      en_cont_list <- offset_query_strings |>
        purrr::map(
          ~api_req(
            query_string = .x,
            api_key = api_key
          )
        ) |>
        unlist(recursive = FALSE)

      return(en_cont_list)

    } else {

      stop(httr::content(resp, encoding = "UTF-8"))

    }

  }
}


#' @title
#' Converts the given XML to a data.table
#'
#' @description
#' This function extracts time series data from an XML document returned by the ENTSOE API.
#' It parses the time series, period start and end times, positions, and price amounts
#' from the XML structure and returns the extracted data as a `data.table`.
#'
#' @param xml_data A parsed XML object. The function expects XML data that follows the ENTSOE
#' Transparency Platform format, particularly including elements like `TimeSeries`, `Period`,
#' and `Point` with associated data.
#'
#' @return
#' A `data.table` with the following columns:
#' \itemize{
#'   \item StartTime: Start time of the period in ISO 8601 format.
#'   \item EndTime: End time of the period in ISO 8601 format.
#'   \item Position: The position of the price point within the period.
#'   \item Price: The price value corresponding to the given position.
#' }
#'
#' @import xml2
#' @import data.table
#'
#' @examples
#' # Assuming `xml_data` is a valid parsed XML object
#' result <- extract_entsoc_data(xml_data)
#' print(result)
#'
#' @noRd
#' @export

extract_xml_dam_data <- function(xml_data) {
  # Define the namespace
  ns <- xml_ns(xml_data)
  ns <- c(ns, default = "urn:iec62325.351:tc57wg16:451-3:publicationdocument:7:3")

  # Find all TimeSeries elements
  time_series <- xml_find_all(xml_data, ".//default:TimeSeries", ns = ns)

  # Initialize a list to store extracted data
  data_list <- list()

  # Loop through each TimeSeries element
  for (series in time_series) {
    # Extract Period information
    periods <- xml_find_all(series, ".//default:Period", ns = ns)

    for (period in periods) {
      # Extract start and end times
      time_interval <- xml_find_first(period, ".//default:timeInterval", ns = ns)
      start_time <- xml_text(xml_find_first(time_interval, ".//default:start", ns = ns))
      end_time <- xml_text(xml_find_first(time_interval, ".//default:end", ns = ns))
      resolution <- xml_text(xml_find_first(period, ".//default:resolution", ns = ns))

      # Extract Points (position and price.amount)
      points <- xml_find_all(period, ".//default:Point", ns = ns)
      for (point in points) {
        position <- as.numeric(xml_text(xml_find_first(point, ".//default:position", ns = ns)))
        price <- as.numeric(xml_text(xml_find_first(point, ".//default:price.amount", ns = ns)))

        # Add to data list
        data_list <- append(data_list, list(data.frame(
          StartTime = start_time,
          EndTime = end_time,
          Position = position,
          Resolution = resolution,
          Price = price
        )))
      }
    }
  }

  # Combine all data into a single DataFrame
  result <- rbindlist(data_list)

  # Return the result
  return(result)
}



#' Check if a Date is a Holiday
#'
#' This function determines if a given date is a holiday based on a fixed list of holidays, weekends, 
#' or the day after Easter.
#'
#' @param date A `Date` object representing the date to check.
#'
#' @return A logical value (`TRUE` or `FALSE`). Returns `TRUE` if the date is a holiday, otherwise `FALSE`.
#'
#' @details
#' The function checks the following conditions to determine if the date is a holiday:
#' - Fixed holidays (e.g., New Year's Day, Christmas).
#' - Weekends (Saturday and Sunday).
#' - The day after Easter.
#'
#' Fixed holidays include:
#' - January 1st (New Year's Day)
#' - January 6th (Epiphany)
#' - April 25th (Liberation Day)
#' - May 1st (Labor Day)
#' - June 2nd (Republic Day)
#' - August 15th (Assumption of Mary)
#' - November 1st (All Saints' Day)
#' - December 8th (Immaculate Conception)
#' - December 25th (Christmas)
#' - December 26th (St. Stephen's Day)
#'
#' @examples
#' # Example 1: Check if a specific date is a holiday
#' is_holiday(as.Date("2023-12-25")) # Christmas - should return TRUE
#'
#' # Example 2: Check if a weekend is a holiday
#' is_holiday(as.Date("2023-07-15")) # Saturday - should return TRUE
#'
#' # Example 3: Check if a non-holiday weekday is a holiday
#' is_holiday(as.Date("2023-07-13")) # Thursday - should return FALSE
#'
#' @importFrom timeDate Easter
#' @export
is_holiday <- function(date) {
  feste <- c(
    "01-01",
    "06-01",
    "25-04",
    "01-05",
    "02-06",
    "15-08",
    "01-11",
    "08-12",
    "25-12",
    "26-12"
  )

  # Check if date is a holiday
  formatted_date <- format(date, "%d-%m")
  easter_date <- as.Date(Easter(year(date)))

  return(formatted_date %in% feste ||
           weekdays(date) %in% c("Saturday", "Sunday") ||
           (easter_date + 1) == date)
}
