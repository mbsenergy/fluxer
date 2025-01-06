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
    warning(sprintf(crayon::red("[ERROR] Failed to fetch data for country %s", country)))
    return(dtw)
  }

  ruby <- fromJSON(content(res, as = "text", encoding = 'UTF-8'))
  last_page <- ruby$last_page

  # Loop through pages from last to first
  for (i in seq(last_page, 1)) {
    url_s_page <- paste0(url_s, "&page=", i)
    res <- GET(url_s_page, add_headers(`x-key` = api_key))

    if (status_code(res) != 200) {
      warning(sprintf(crayon::red("[ERROR] Failed to fetch data for country %s on page %d", country, i)))
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

  message(crayon::green('[OK] File successfully processed.'))

  return(dts)
}



#' Download and Process ALBA Data
#'
#' This function logs into the ALBA energy platform, downloads a data file, and processes its content from multiple Excel sheets.
#'
#' @param from_date A `Date` object indicating the start date for data processing (not directly used in this code but can be added for filtering).
#' @param to_date A `Date` object indicating the end date for data processing (not directly used in this code but can be added for filtering).
#' @param output_dir A string specifying the directory where the downloaded file will be saved.
#' @param username A string containing the username for authentication.
#' @param password A string containing the password for authentication.
#'
#' @return A list containing:
#' \item{sheet_data_list}{A list of `data.table` objects, one for each sheet in the Excel file.}
#' \item{combined_data}{A combined `data.table` containing all data from the Excel sheets.}
#'
#' @details
#' The function performs the following steps:
#' 1. Logs into the ALBA energy platform using provided credentials.
#' 2. Downloads the Excel file containing data.
#' 3. Processes each sheet of the Excel file:
#'    - Reads the data.
#'    - Transforms it into long format.
#'    - Removes rows with missing values.
#'    - Adds metadata about the sheet name.
#' 4. Combines data from all sheets into a single `data.table` for easier analysis.
#'
#' @import httr
#' @import rvest
#' @import xml2
#' @import data.table
#' @import openxlsx
#' @import magrittr
#' @importFrom data.table setDT melt rbindlist
#' @examples
#' \dontrun{
#' # Define parameters
#' from_date <- as.Date("2023-01-01")
#' to_date <- as.Date("2023-12-31")
#' output_dir <- "data"
#' username <- "your_username"
#' password <- "your_password"
#'
#' # Call the function
#' result <- alba_download_data(from_date, to_date, output_dir, username, password)
#'
#' # Access individual sheet data
#' sheet_data_list <- result$sheet_data_list
#'
#' # Access combined data
#' combined_data <- result$combined_data
#' }
#' @export
alba_download_data_power <- function(from_date, to_date, type, output_dir, username, password, raw = FALSE) {
  # Login and download URLs
  login_url <- "https://www.geeo.energy/login/?lang=it"
  ajax_url <- "https://www.geeo.energy/wp-admin/admin-ajax.php"
  download_url <- "https://www.geeo.energy/download-geeodatapoweritalian.php"

  # Login credentials
  login_data <- list(
    username = username,
    password = password,
    action = "ajaxlogin",
    security = ""
  )

  # Start session and get login page
  session <- session(login_url)
  form <- html_form(session)[[1]]
  login_data$security <- form$fields[["security"]]$value

  # Perform login
  response <- POST(
    ajax_url,
    body = login_data,
    encode = "form",
    add_headers(
      `X-Requested-With` = "XMLHttpRequest",
      `Content-Type` = "application/x-www-form-urlencoded",
      `Referer` = login_url
    )
  )

  # Download the file
  download_response <- GET(download_url, session$config)
  content_disposition <- headers(download_response)["content-disposition"]
  
  # Extract filename from the content-disposition header
  filename <- gsub(" ", "-", regmatches(content_disposition, regexpr('".*"', content_disposition)))
  filename <- gsub('"', '', filename)

  # Define the file path
  file_path <- file.path(output_dir, filename)

  # Write the downloaded file to the specified directory
  writeBin(content(download_response, "raw"), file_path)

  if(isFALSE(raw)) {

    # Process the downloaded file
    result_data <- process_alba_xlsx(file_path, from_date, to_date, type = "electricity")

    # Delete the file after processing
    file.remove(file_path)
    message(paste(crayon::green("[OK] File succesfully processed")))

    # Return the processed data
    return(result_data)
    
  } else {

    message(paste(crayon::green("[OK] File saved:", file_path)))
    return(TRUE)

  }
  }

#' Download and process gas price data from GEEO Energy
#'
#' This function logs into the GEEO Energy website, downloads the gas price data,
#' processes the data using the `process_xlsx` function, and then deletes the
#' downloaded file after processing.
#'
#' @param from_date A Date object or a character string in "YYYY-MM-DD" format specifying the start date for filtering the data.
#' @param to_date A Date object or a character string in "YYYY-MM-DD" format specifying the end date for filtering the data.
#' @param output_dir A character string specifying the output directory where the file will be saved.
#' @param username A character string specifying the username for login.
#' @param password A character string specifying the password for login.
#'
#' @return A `data.table` containing the processed gas price data.
#'
#' @details
#' This function logs into the GEEO Energy platform, retrieves the gas price data as an Excel file,
#' processes it to extract relevant data within the specified date range, and then deletes the file
#' after processing.
#'
#' @import httr
#' @import rvest
#' @import xml2
#' @import data.table
#' @import openxlsx
#' @import magrittr
#'
#' @export
alba_download_data_gas <- function(from_date, to_date, type, output_dir, username, password, raw = FALSE) {
  # Login and download URLs
  login_url <- "https://www.geeo.energy/login/?lang=it"
  ajax_url <- "https://www.geeo.energy/wp-admin/admin-ajax.php"
  download_url <- "https://www.geeo.energy/download-geeodataitalian.php"

  # Login credentials
  login_data <- list(
    username = username,
    password = password,
    action = "ajaxlogin",
    security = ""
  )

  # Start session and get login page
  session <- session(login_url)
  form <- html_form(session)[[1]]
  login_data$security <- form$fields[["security"]]$value

  # Perform login
  response <- POST(
    ajax_url,
    body = login_data,
    encode = "form",
    add_headers(
      `X-Requested-With` = "XMLHttpRequest",
      `Content-Type` = "application/x-www-form-urlencoded",
      `Referer` = login_url
    )
  )

  # Download the file
  download_response <- GET(download_url, session$config)
  content_disposition <- headers(download_response)["content-disposition"]
  
  # Extract filename from the content-disposition header
  filename <- gsub(" ", "-", regmatches(content_disposition, regexpr('".*"', content_disposition)))
  filename <- gsub('"', '', filename)

  # Define the file path
  file_path <- file.path(output_dir, filename)

  # Write the downloaded file to the specified directory
  writeBin(content(download_response, "raw"), file_path)

  if(isFALSE(raw)) {
    # Process the downloaded file
    result_data <- process_alba_xlsx(file_path, from_date, to_date, type = "gas")

    # Delete the file after processing
    file.remove(file_path)

    message(paste(crayon::green("[OK] File succesfully processed")))

    # Return the processed data
    return(result_data)
    
  } else {

    message(paste(crayon::green("[OK] File saved:", file_path)))
    return(TRUE)

  }

}

#' Process Excel file and extract relevant data
#'
#' This function processes an Excel file containing multiple sheets, extracts
#' relevant data based on the type of commodity (gas or electricity), filters
#' the data within the specified date range, and combines it into a single
#' `data.table`. The data is reshaped, cleaned, and additional columns are added
#' for further analysis.
#'
#' @param filename A character string specifying the path to the Excel file to be processed.
#' @param from_date A Date object or a character string in "YYYY-MM-DD" format specifying the start date for filtering the data.
#' @param to_date A Date object or a character string in "YYYY-MM-DD" format specifying the end date for filtering the data.
#' @param type A character string specifying the type of commodity. It can be either "gas" or "electricity".
#' 
#' @return A `data.table` containing the processed data, with columns:
#'   - `DATE`: The date of the observation.
#'   - `TYPE`: The commodity type (e.g., "PSV", "COAL").
#'   - `VARIABLE`: The name of the variable (e.g., "PSV MW").
#'   - `VALUE`: The value of the variable for the given date.
#'
#' @details
#' This function processes the following sheets for "gas" type: "PSV MW", "GR04 MW", "TTF MW", 
#' "GR07 MW", "Baum MW", "NCG MW", "PEG MW". For "electricity" type, the following sheets 
#' are processed: "Elettricita Italia", "Elettricita Francia", "Elettricita Germania", "COAL", 
#' and "CO2". Data is filtered by the `from_date` and `to_date` parameters, reshaped from wide 
#' to long format, and combined into a single `data.table`.
#'
#' @import data.table
#' @importFrom openxlsx read.xlsx
#' @importFrom magrittr %>%
#'
#' @examples
#' # Example usage:
#' filename <- "path/to/your/file.xlsx"
#' from_date <- Sys.Date() - 7
#' to_date <- Sys.Date()
#' type <- "electricity"
#' processed_data <- process_xlsx(filename, from_date, to_date, type)
process_alba_xlsx <- function(filename, from_date, to_date, type) {
  message(crayon::green("Opening file..."))
  sheets <- getSheetNames(filename)
  message(crayon::green("[OK] File opened!"))
  
  # Initialize an empty list to store the data tables for each sheet
  result_list <- list()
  
  if (type == "gas") {
    relevant_sheets <- c("PSV MW", "GR04 MW", "TTF MW", "GR07 MW", "Baum MW", "NCG MW", "PEG MW")
    combust <- list("PSV MW" = "PSV", "GR04 MW" = "GR04", "TTF MW" = "TTF", "GR07 MW" = "GR07",
                    "Baum MW" = "BAUM", "NCG MW" = "NCG", "PEG MW" = "PEG")
  } else {
    relevant_sheets <- c("Elettricita Italia", "Elettricita Francia", "Elettricita Germania", "COAL", "CO2")
    combust <- list("Elettricita Italia" = "ITA", "Elettricita Francia" = "FRA", "Elettricita Germania" = "GER",
                    "COAL" = "COAL", "CO2" = "CO2")
  }
  
  for (sheet_name in sheets) {
    if (sheet_name %in% relevant_sheets) {
      message(paste(crayon::green("Processing sheet:", sheet_name)))
      data <- read.xlsx(filename, sheet = sheet_name, startRow = 2, detectDates = TRUE)
      setDT(data)
      
      # Filter data within the date range
      data <- data[Data >= as.Date(from_date) & Data <= as.Date(to_date)]
      
      if (nrow(data) > 0) {
        melted_data <- melt(data, id.vars = "Data", variable.factor = FALSE, variable.name = "header", value.name = "value")
        melted_data <- melted_data[!is.na(value) & value != ""]
        
        # Special handling for "CO2" sheet
        if (sheet_name == "CO2") {
          melted_data[, header := ifelse(header != "Current year",
                                         paste0("AS", substr(as.character(header), nchar(as.character(header)) - 1, nchar(as.character(header)))),
                                         header)]
        }
        
        # Add additional columns
        melted_data[, comb := combust[[sheet_name]]]
        
        # Store the data from each sheet into the result list
        result_list[[sheet_name]] <- melted_data
        
        # Print a preview of the sheet's processed data
        print(head(melted_data))  # Replace with actual save logic if needed
      } else {
        message(paste(crayon::yellow("[WARNING] No relevant data found in sheet:", sheet_name)))
      }
    }
  }
  
  # Combine all the data.tables in the result list into a single data.table
  combined_data <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
  
  # Return the combined data.table
  setnames(combined_data, old = c('Data', 'header', 'value', 'comb'), new = c('DATE', 'VARIABLE', 'VALUE', 'TYPE'))
  setcolorder(combined_data, c('DATE', 'TYPE', 'VARIABLE', 'VALUE')) 
  
  return(combined_data)
}
