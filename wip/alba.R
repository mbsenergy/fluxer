


# Load required libraries
library(httr)
library(rvest)
library(openxlsx)
library(data.table)

output_dir = "data"
username = "checchi"
password = "2bczpsNH"
from_date = Sys.Date() - 5
to_date = Sys.Date()

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


# Define main function
process_data <- function(from_date, to_date, output_dir, username, password) {
  filename <- ""

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
    filename <- gsub(" ", "-", regmatches(content_disposition, regexpr('".*"', content_disposition)))
    filename <- gsub('"', '', filename)

    file_path <- file.path(output_dir, filename)
    writeBin(content(download_response, "raw"), file_path)

    print(paste("File saved:", file_path))

      sheet_names <- getSheetNames(file_path)
      
      # Initialize a list to store data from each sheet
      sheet_data_list <- list()
      
      # Loop through each sheet
      for (sheet in sheet_names) {
        # Read data from the current sheet
        sheet_data <- read.xlsx(file_path, sheet = sheet, detectDates = TRUE, startRow = 2)
        setDT(sheet_data)
        
        sheet_data_lg = melt(
          sheet_data, id.vars = 'Data', 
          variable.name = 'VARIABLE',
          value.name = 'VALUE',
        variable.factor = FALSE)
        
        # Clean or process data as needed (e.g., remove empty rows, set column names)
        sheet_data <- sheet_data[complete.cases(sheet_data)]  # Remove rows with NA
        sheet_data[, Sheet := sheet]  # Add a column to indicate the sheet name
        
        # Append to the list
        sheet_data_list[[sheet]] <- sheet_data
      }
      
      # Combine all sheets into one data.table for easy analysis (if needed)
      combined_data <- rbindlist(sheet_data_list, use.names = TRUE, fill = TRUE)
      
      # Return the list of sheets and the combined data
      list(
        sheet_data_list = sheet_data_list,
        combined_data = combined_data
      )

}

# Example usage
# process_data("/work", "checchi", "2bczpsNH", Sys.Date() - 5, Sys.Date())


# Example usage
process_data()
