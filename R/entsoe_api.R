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
#'
#'
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
#' @import lubridate
#' @import ggplot2
#'
#' @examples
#' # Example usage to retrieve DAM prices for Italy (North)
#' country <- "Italy (North)"
#' from_data <- Sys.Date() - 365
#' to_data <- Sys.Date()
#' dam_prices <- entsoe_dam_prices(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY'))
#' print(dam_prices)
#' @export
#'
entsoe_dam_prices = function(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY'), verbose=FALSE, plot=FALSE) {

    if (identical(api_key, "")) {
        cat(yellow("Warning: Missing API key! Please set 'ENTSOE_KEY' in your environment.\n"))
    }

    if (is.character(from_data)) from_data <- as.Date(from_data)
    if (is.character(to_data)) to_data <- as.Date(to_data)

    # Check again to ensure both are now Dates
    if (!inherits(from_data, "Date") | !inherits(to_data, "Date")) {
        stop("Error: Both 'from_data' and 'to_data' must be of class Date or convertible to Date.")
    }

    from_data <- from_data %>% as.Date()
    to_data <- to_data %>% as.Date()

    if (to_data - from_data < months(1)) {
      stop("Error: The difference between start_date and to_date is less than a month!")
    }

    # Generate a sequence of months from start to end
    # Generate a sequence of months from start to end
    month_starts <- seq(from_data, to_data, by = "month")
    month_starts[1] <- as.Date(paste0(year(month_starts[1]), "-", month(month_starts[1]), "-01"))
    month_starts[-1] <- as.Date(paste0(year(month_starts[-1]), "-", month(month_starts[-1]), "-01"))
    month_ends <- as.Date(pmin(lubridate::ceiling_date(month_starts, "month") - 1, to_data))

    # Create data.table
    dt_dates <- data.table(FROM = month_starts, TO = month_ends)

    if (dt_dates[nrow(dt_dates), TO] != to_data) {

        last_full_month_end <- as.Date(pmin(lubridate::ceiling_date(month_starts[nrow(dt_dates)], "month") - 1, to_data))
        new_row <- data.table(FROM = last_full_month_end + 1, TO = to_data)
        dt_dates[nrow(dt_dates), TO := last_full_month_end]
        dt_dates <- rbind(dt_dates, new_row)
    }

    results <- lapply(1:nrow(dt_dates), function(i) {
        api_entsoe_dam_prices(country, dt_dates$FROM[i], dt_dates$TO[i], api_key)
    })

    # If entsoe_api returns a data.frame or list, you can combine results:
    results_combined <- rbindlist(results, fill = TRUE)
    results_combined[, DATETIME := as.POSIXct(paste(DATE, HOUR), format = "%Y-%m-%d %H")]
    setorder(results_combined, DATETIME)


    if(isTRUE(verbose)) {
        print(results_combined)
    }

    if(isTRUE(plot)) {

        P1 =
        ggplot2::ggplot(results_combined, aes(x = DATETIME, y = VALUE, color = COUNTRY)) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            labs(title = "Electricity Data Over Time",
                 x = "Datetime",
                 y = "Value") +
            ggplot2::theme_minimal()

        print(P1)

    }


    return(results_combined)

}



#' @title
#' INTERNAL - API Retrieve Day-Ahead Market Prices for a Specific Country
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
#' @export
#'
api_entsoe_dam_prices = function(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY')) {

  # Translate the country to the EIC code using the entsoe_countries mapping
  entsoe_domain = entsoe_countries[COUNTRY == country]$CODE_EIC

  # Format the start and end period times for the API request
  period_start <- paste0(format(as.Date(from_data) - 1, "%Y%m%d"), "2300")
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
  result <- extract_xml_dam_data(res_content)

  # Process the extracted data and format it
  DT = result[, .(CODE_ENTSOE = country, DATE = format(suppressWarnings(strptime(EndTime, "%Y-%m-%dT%H:%MZ")), "%Y-%m-%d"),
                  HOUR = Position, RESOLUTION = Resolution, VALUE = Price, UNIT = 'EUR')]

  DT = merge(DT, entsoe_countries[, .(COUNTRY, CODE_ENTSOE)], by = 'CODE_ENTSOE', all.x = TRUE)

  full_hours = DT[, .(HOUR = 1:24), by = .(COUNTRY, CODE_ENTSOE, DATE, RESOLUTION, UNIT)]
  DT = merge(full_hours, DT, by = c('COUNTRY', 'CODE_ENTSOE', 'DATE', 'HOUR', 'RESOLUTION', 'UNIT'), all.x = TRUE)
  DT[, VALUE := zoo::na.locf(VALUE, na.rm = FALSE), by = .(COUNTRY, CODE_ENTSOE, DATE)]

  setcolorder(DT, neworder = c('COUNTRY', 'CODE_ENTSOE', 'DATE', 'HOUR', 'RESOLUTION', 'VALUE', 'UNIT'))

  # Display a success message with formatting using glue and crayon
  message <- glue("[{crayon::bold('OK')}] DATA for {crayon::bold(country)} from {crayon::bold(from_data)} to {crayon::bold(to_data)} retrieved correctly")
  cat(crayon::green(message), "\n")

  # Return the extracted and formatted data as a data.table
  return(DT)
}




#' @title
#' Retrieve Actual Generation by Production Type for a Specific Country
#'
#' @description
#' This function retrieves actual generation by production type data for a specific country from the ENTSOE Transparency Platform API.
#' The function fetches generation data for a given time range and country, processes the data, and formats it into a `data.table`.
#'
#' @param country A character string specifying the country for which to retrieve generation data. The country should match an entry in the `entsoe_countries` table.
#' @param from_data A Date object specifying the start date for the data range. Default is 365 days before the current date.
#' @param to_data A Date object specifying the end date for the data range. Default is the current date.
#' @param api_key A character string containing the ENTSOE API key. It can be passed directly or retrieved from the environment variable `ENTSOE_KEY`.
#'
#' @return
#' A `data.table` containing the formatted generation data for the specified country and time range. The data table includes:
#' \itemize{
#'   \item `DATE`: The date in `YYYY-MM-DD` format.
#'   \item `HOUR`: The hour of the day (position in the data).
#'   \item `PRODUCTION_TYPE`: The type of production (e.g., solar, wind, nuclear).
#'   \item `VALUE`: The generation value in MW.
#'   \item `UNIT`: The unit of the generation value (always 'MW').
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
#' # Example usage to retrieve generation data for Italy (North)
#' country <- "Italy (North)"
#' from_data <- Sys.Date() - 365
#' to_data <- Sys.Date()
#' generation_data <- entsoe_generation_by_type(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY'))
#' print(generation_data)
#'
#' @export
entsoe_generation_by_type = function(country, from_data, to_data, api_key = Sys.getenv('ENTSOE_KEY')) {

    if (is.null(country)) {stop("The argument 'country' is missing!")}
    if (is.null(from_data)) {stop("The argument 'from_data' is missing!")}
    if (is.null(to_data)) {stop("The argument 'to_data' is missing!")}

  entsoe_domain = entsoe_countries[CODE_ENTSOE == country]$CODE_EIC

    # Format the start and end period times for the API request

    from_data = as.Date(from_data, format = "%d/%m/%Y")
    to_data = as.Date(to_data, format = "%d/%m/%Y") + 1 # +1 to include the last date

    period_start = as.character(
      year(from_data) * 10^8 + month(from_data) * 10^6 + as.numeric(format(from_data, "%d")) * 10^4)

    period_end = as.character(
      year(to_data) * 10^8 + month(to_data) * 10^6 + as.numeric(format(to_data, "%d")) * 10^4)

    eic = entsoe_domain

    query_string <- paste0(
      "documentType=A75",
      "&processType=A16",
      "&in_Domain=", eic,
      "&periodStart=", period_start,
      "&periodEnd=", period_end
    )

    xml_file = api_req(query_string = query_string,
                       api_key = api_key)
    xml_text <- as.character(xml_file)

    writeLines(xml_text, "output.txt")

    ns <- xml_ns(xml_file)
    ns <- c(ns = as.character(ns))

    # check

    error_xml_file <- as.character(xml_find_all(xml_file, "//ns:text", ns))
    if(length(error_xml_file) != 0) {
      # message(error_xml_file)
      stop(error_xml_file)
    }

    # periods <- xml_find_all(xml_file, "//ns:Period", ns)
    time_series <- xml_find_all(xml_file, "//ns:TimeSeries", ns)

    dt_master <- data.table()

    for (i in seq_along(time_series)) {

      periods <- xml_find_all(time_series[i], ".//ns:Period", ns)

      # Extract timeInterval start and end
      time_interval <- xml_find_first(periods, "./ns:timeInterval", ns)
      start_time <- xml_text(xml_find_first(time_interval, "./ns:start", ns))
      end_time <- xml_text(xml_find_first(time_interval, "./ns:end", ns))

      # Extract uom
      uom <- xml_text(xml_find_first(time_series[i], "./ns:quantity_Measure_Unit.name", ns))

      # Extract asset type
      asset_type <- xml_text(xml_find_first(time_series[i], "./ns:MktPSRType", ns))
      asset_type_name <- asset_types[CODE == asset_type,]$DEFINITION[[1]]

      # Create date vector
      start_datetime <- as.POSIXct(start_time, format="%Y-%m-%dT%H:%MZ", tz="UTC")
      end_datetime <- as.POSIXct(end_time, format="%Y-%m-%dT%H:%MZ", tz="UTC")


      resolution <- xml_text(xml_find_first(periods, "./ns:resolution", ns))
      granularity <- fifelse(resolution == "PT15M", "15 min",
                             fifelse(resolution == "PT60M", "1 hour",
                                     fifelse(resolution == "PT30M", "30 min","ERROR")))

      datetime_vector <- seq(start_datetime, end_datetime, by = granularity)
      datetime_vector <- datetime_vector[-length(datetime_vector)]

      # Extract Points (position and quantity)
      points <- xml_find_all(periods, "./ns:Point", ns)
      positions <- as.numeric(xml_text(xml_find_all(points, "./ns:position", ns)))
      quantities <- as.numeric(xml_text(xml_find_all(points, "./ns:quantity", ns)))

      if(length(datetime_vector) != length(quantities)) {stop("different length")}

      # Combine extracted data into a data table
      dt_loop <- data.table(
        date = datetime_vector,
        position = positions,
        value = quantities,
        asset_type = asset_type_name,
        uom = uom,
        zone = entsoe_domain,
        data = "actual_generation_type"
      )

      dt_master <- rbind(dt_master, dt_loop)

    }

    entsoe_countries[CODE_ENTSOE == country]
    dt_master[, COUNTRY := entsoe_countries[CODE_ENTSOE == country]$COUNTRY]
    dt_master[, CODE_ENTSOE := entsoe_countries[CODE_ENTSOE == country]$CODE_ENTSOE]
    # setnames(dt_master, )

    message <- glue("[{crayon::bold('OK')}] DATA for {crayon::bold(country)} from {crayon::bold(from_data)} to {crayon::bold(to_data)} retrieved correctly")
    cat(crayon::green(message), "\n")

    return(dt_master)

  }
