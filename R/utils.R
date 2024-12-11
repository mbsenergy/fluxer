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
extract_xml_data <- function(xml_data) {
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
