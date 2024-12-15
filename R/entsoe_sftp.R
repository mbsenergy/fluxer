#' Retrieve and Process ENTSO-E Actual Generation Data
#'
#' This function retrieves and processes the ENTSO-E actual generation data for a specified year and month.
#' It supports an option to either retain or delete the raw CSV file after processing based on the `raw` argument.
#' It also allows specifying the output folder for the downloaded CSV file.
#'
#' @param year_data Numeric. The year for which to download the ENTSO-E data (e.g., 2024).
#' @param month_data Numeric. The month for which to download the ENTSO-E data (e.g., 10 for October).
#' @param raw Logical. If `TRUE`, keeps the downloaded CSV file after processing. If `FALSE`, the CSV file is deleted after being read. Default is `FALSE`.
#' @param api_key Character. Your ENTSO-E API key. Passed to `entsoe_download_file` for authenticating the download request. Default is `Sys.getenv('ENTSOE_KEY')`.
#' @param output_folder Character. The folder where the raw CSV file will be saved. Default is `tempdir()`.
#'
#' @return A `data.table` in long format with the following columns:
#' \describe{
#'   \item{DATE}{The date of the data point.}
#'   \item{TIME}{The time of the data point in `HH:MM` format.}
#'   \item{HOUR}{The hour extracted from the `DATETIME` column as an integer.}
#'   \item{RESOLUTION}{The resolution code (e.g., `quarter-hour`, `hourly`).}
#'   \item{CODE_MAP}{The map code (e.g., country, bidding zone).}
#'   \item{CODE_EIC}{The EIC code for the area (e.g., grid or market region).}
#'   \item{PRODUCTION_TYPE}{The type of power generation (e.g., "Fossil Oil", "Wind Onshore").}
#'   \item{ASSET_CATEGORY}{The category of the asset (e.g., "Thermal", "Wind", etc.).}
#'   \item{ACTUAL_GENERATION}{The actual generation output for the given time period.}
#'   \item{ACTUAL_CONSUMPTION}{The actual consumption for the given time period.}
#'   \item{UPDATETIME}{The last update time for the data entry.}
#'   \item{VARIABLE}{The variable being measured, either "ACTUAL_GENERATION" or "ACTUAL_CONSUMPTION".}
#'   \item{VALUE}{The value for the corresponding variable.}
#' }
#'
#' @examples
#' \dontrun{
#' # Download and process the data for October 2024, keeping the CSV file
#' result_raw <- entsoe_actual_generation(2024, 10, raw = TRUE)
#'
#' # Download and process the data for October 2024, deleting the CSV file after processing
#' result <- entsoe_actual_generation(2024, 10)
#' }
#'
#' @export
entsoe_actual_generation = function(year_data, month_data, raw = FALSE, api_key = Sys.getenv('ENTSOE_KEY'), output_folder = tempdir()) {

    # Ensure the output folder exists
    if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
    }

    # Define the output file path in the specified folder
    OUT_TEMP = file.path(output_folder, paste0('ACTUAL_GEN_', as.integer(year_data), as.integer(month_data), '.csv'))

    # Download the file using the entsoe_download_file function
    entsoe_download_file(basis_name = 'AggregatedGenerationPerType_16.1.B_C', year = as.integer(year_data), month = as.integer(month_data), output_file = OUT_TEMP)

    # Read the downloaded CSV file into a data.table
    actual_gen = fread(OUT_TEMP)

    # Clean and transform the data
    actual_gen = actual_gen[, .(DATETIME = DateTime, RESOLUTION = ResolutionCode,
                                CODE_EIC = AreaCode, PRODUCTION_TYPE = ProductionType,
                                CODE_MAP = MapCode,
                                ACTUAL_GENERATION = ActualGenerationOutput,
                                ACTUAL_CONSUMPTION = ActualConsumption,
                                UPDATETIME = UpdateTime)]

    actual_gen[, DATETIME := as.POSIXct(DATETIME, format = "%Y-%m-%d %H:%M:%OS")]
    actual_gen[, DATE := as.Date(DATETIME)]
    actual_gen[, HOUR := as.integer(format(DATETIME, "%H"))]
    actual_gen[, TIME := format(DATETIME, "%H:%M")]

    # Merge with asset_types to include asset category
    actual_gen = merge(actual_gen, asset_types[, .(PRODUCTION_TYPE, ASSET_CATEGORY)], by = 'PRODUCTION_TYPE')

    # Drop the original DATETIME column and reorder columns
    actual_gen[, DATETIME := NULL]
    setcolorder(actual_gen, c('DATE', 'TIME', 'HOUR', 'RESOLUTION', 'CODE_MAP', 'CODE_EIC', 'PRODUCTION_TYPE', 'ASSET_CATEGORY', 'ACTUAL_GENERATION', 'ACTUAL_CONSUMPTION'))

    # Reshape the data into long format
    actual_gen = melt(actual_gen, id.vars = c('DATE', 'TIME', 'HOUR', 'RESOLUTION', 'CODE_MAP', 'CODE_EIC', 'PRODUCTION_TYPE', 'ASSET_CATEGORY', 'UPDATETIME'),
                      variable.name = 'VARIABLE', value.name = 'VALUE', variable.factor = FALSE)

    # If raw is FALSE, delete the downloaded CSV file
    if (!raw) {
        file.remove(OUT_TEMP)
    }

    return(actual_gen)
}


#' Download a File from ENTSO-E SFTP Server
#'
#' This function downloads a specified file from the ENTSO-E SFTP server using the provided credentials.
#'
#' @param basis_name A character string specifying the high-level folder name of the file.
#' @param year An integer specifying the year of the file to retrieve.
#' @param month An integer specifying the month of the file to retrieve (e.g., 1 for January).
#' @param user A character string for the username for SFTP authentication.
#' @param password A character string for the password for SFTP authentication.
#' @param output_file A character string specifying the local file path where the downloaded file will be saved.
#'
#' @return Logical value indicating whether the file was downloaded successfully.
#'
#' @examples
#' \dontrun{
#' # Download a file for specified parameters
#' download_successful <- download_entsoe_file(basis_name = "example_folder", year = 2023, month = 1,
#'                                               user = Sys.getenv("ENTSOE_USER"),
#'                                               password = Sys.getenv("ENTSOE_PASSWORD"),
#'                                               output_file = "local_file_path.csv")
#' }
#'
#' @export
entsoe_download_file <- function(basis_name, year, month, user = Sys.getenv("ENTSOE_USER"), password = Sys.getenv("ENTSOE_PASSWORD"), output_file) {

    # Construct the file URL
    file_url <- paste0("sftp://sftp-transparency.entsoe.eu/TP_export/",
                       basis_name, "/",
                       year, "_", sprintf("%02d", month), "_", basis_name, ".csv")

    # Create a curl handle
    h <- curl::new_handle()
    curl::handle_setopt(h,
                        .list = list(
                            httpauth = 1,
                            userpwd = paste0(user, ":", password),
                            ftp_use_epsv = FALSE))  # Set to FALSE for SFTP

    # Perform the download
    result <- curl::curl_download(file_url, output_file, handle = h)

    # Check if the download was successful
    if (file.exists(output_file)) {
        message("File downloaded successfully: ", output_file)
        return(TRUE)
    } else {
        message("Failed to download the file.")
        return(FALSE)
    }
}




#' Create URL Folder Request to ENTSO-E SFTP Server
#'
#' This function creates and sends a request to the ENTSO-E SFTP server's transparency platform,
#' retrieving the list of available folders or files. It authenticates using provided credentials
#' (`user` and `psw`) for secure FTP access.
#'
#' @param user A character string with the username for ENTSO-E SFTP server authentication.
#' @param psw A character string with the password for ENTSO-E SFTP server authentication.
#'
#' @details This function uses `curl` to handle the HTTP request for secure FTP access.
#' Ensure that the `user` and `psw` parameters are provided with valid ENTSO-E credentials.
#'
#' @return A response object containing the contents of the specified SFTP directory,
#' as retrieved from the ENTSO-E transparency platform.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' entsoe_create_url_folders(user = "your_username", psw = "your_password")
#' }
#'
#' @export
entsoe_create_url_folders = function(user = Sys.getenv('ENTSOE_USER'), psw = Sys.getenv('ENTSOE_PASSWORD')) {

    h = curl::new_handle()
    curl::handle_setopt(h, .list = list(httpauth = 1, userpwd = paste0(user, ":", psw), httpget = TRUE))
    req = curl::curl_fetch_memory(url = "sftp://sftp-transparency.entsoe.eu/TP_export/", h)

    req
}

#' Retrieve Folder Contents from ENTSO-E SFTP Server
#'
#' This function requests and retrieves the contents of a specified folder on the ENTSO-E SFTP server,
#' given a folder name (`basis_name`). It authenticates using credentials stored in environment
#' variables (`ENTSOE_USER` and `ENTSOE_PASSWORD`).
#'
#' @param basis_name A character string specifying the name of the folder within the ENTSO-E SFTP server's transparency platform.
#'
#' @details This function utilizes `curl` to securely access the specified folder via SFTP.
#' Ensure the environment variables `ENTSOE_USER` and `ENTSOE_PASSWORD` contain valid credentials.
#'
#' @return A response object containing the contents of the specified folder on the SFTP server.
#'
#' @export
entsoe_create_url_files = function(basis_name) {

    h = curl::new_handle()
    curl::handle_setopt(h, .list = list(httpauth = 1, userpwd = paste0(Sys.getenv("ENTSOE_USER"), ":", Sys.getenv("ENTSOE_PASSWORD")), httpget = TRUE))
    req = curl::curl_fetch_memory(url = paste0("sftp://sftp-transparency.entsoe.eu/TP_export/", basis_name, "/"), h)

    req
}


#' Retrieve a Specific File from ENTSO-E SFTP Server
#'
#' This function requests and retrieves a specific CSV file from the ENTSO-E SFTP server based on
#' a folder (`basis_name`), year, and month. It authenticates using credentials stored in environment
#' variables (`ENTSOE_USER` and `ENTSOE_PASSWORD`).
#'
#' @param basis_name A character string specifying the folder name within the ENTSO-E SFTP server's transparency platform.
#' @param year A numeric or character string specifying the year of the file to be retrieved.
#' @param month A numeric or character string specifying the month of the file to be retrieved.
#'
#' @details The function constructs the file path from `basis_name`, `year`, and `month` and uses `curl`
#' to retrieve the specified CSV file from the server.
#'
#' @return A response object containing the contents of the specified file.
#'
#' @export
entsoe_create_url_file = function(basis_name, year, month) {

    h = curl::new_handle()
    curl::handle_setopt(h, .list = list(httpauth = 1, userpwd = paste0(Sys.getenv("ENTSOE_USER"), ":", Sys.getenv("ENTSOE_PASSWORD")), httpget = TRUE))
    req = curl::curl_fetch_memory(url = paste0("sftp://sftp-transparency.entsoe.eu/TP_export/", basis_name, "/", year, "_", month, "_", basis_name, ".csv"), h)

    req
}



#' List Top-Level Folder Links from ENTSO-E SFTP Server
#'
#' This function retrieves and lists the top-level folder links from the ENTSO-E SFTP server's transparency platform.
#' It uses the `entsoe_create_url_folders` function to fetch the raw folder list and processes it using `parse_ftp_links`.
#'
#' @details The function makes a request to the ENTSO-E SFTP server, retrieves folder content as raw text,
#' and converts it into a structured format using `parse_ftp_links`. The resulting data frame lists folder names
#' and relevant details.
#'
#' @return A data frame containing the names and metadata of top-level folders on the ENTSO-E SFTP server.
#'
#' @examples
#' \dontrun{
#' # List the folders:
#' head(entsoe_list_folders())
#' }
#'
#' @export
entsoe_list_folders = function() {

    req = entsoe_create_url_folders()
    con = rawToChar(req$content)
    con_df = parse_ftp_links(con)
    con_df
}


parse_ftp_links <- function(x) {
    x <- textConnection(x)
    df_links <- read.fwf(file = x,
                         widths = c(10, 5, 4, 10, 14, 13, 60),
                         header = FALSE,
                         stringsAsFactors = FALSE)

    colnames(df_links) <- c("perm", "unknown1", "unknown2", "unknown3", "size", "date", "link")
    dt_links <- as.data.table(df_links)

    dt_links[, time_or_year := substr(date, nchar(date) - 4, nchar(date))]

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    dt_links[, time_and_year := ifelse(grepl(" [0-9]{4}$", time_or_year),
                                       time_or_year,
                                       paste(current_year, time_or_year))]

    dt_links[, date := as.Date(paste(format(Sys.Date(), "%Y"), date), format = "%Y %b %d %H:%M")]
    dt_links <- dt_links[unknown1 == 2]

    return(dt_links)
}


parse_links <- function(x) {

    x <- strsplit(x, "\n")[[1]]

    x <-
        lapply(x, function(z) {
            perm <- strex(z, "^[a-z-]+")
            dir <- strex(z, "[0-9]\\s[a-z]+")
            group <- strex(z, "csdb-ops|1005")
            size <- strexg(z, "[0-9]{2,}")[[1]][2]
            date <- strex(z, "[A-Za-z]{3}\\s+[0-9]{1,2}\\s+[0-9]{2}:[0-9]{2}|[A-Za-z]{3}\\s+[0-9]{1,2}\\s+[0-9]{4}")
            links <- strex(z, "[A-Za-z0-9_-]+$")
            tmp <- list(perm = perm, dir = dir, group = group, size = size,
                        date = date, links = links)
            tmp[vapply(tmp, length, 1) == 0] <- ""
            tmp
        })

    x
}


#' List Files within a Specified Folder on ENTSO-E SFTP Server
#'
#' This function retrieves and lists files from a specified folder (`basis_name`) on the ENTSO-E SFTP server's transparency platform.
#' It uses the `entsoe_create_url_files` function to fetch the file listing and processes the raw content with `parse_ftp_files`.
#'
#' @param basis_name A character string specifying the name of the folder from which to retrieve the file listing.
#'
#' @details The function sends a request to the ENTSO-E SFTP server, retrieves the list of files as raw text,
#' and converts it into a structured format using `parse_ftp_files`. Ensure that `basis_name` corresponds to an
#' existing folder on the server.
#'
#' @return A data frame containing information about files within the specified folder on the ENTSO-E SFTP server.
#' The data frame includes file names and any other available metadata.
#'
#' @examples
#' \dontrun{
#' # List files in a folder named "example_folder":
#' head(entsoe_list_files(basis_name = "OutagesPU"))
#' }
#'
#' @export
entsoe_list_files = function(basis_name) {

    req = entsoe_create_url_files(basis_name)
    con = rawToChar(req$content)
    con_df = parse_ftp_files(con)
    con_df
}


parse_ftp_files <- function(x) {
    x <- textConnection(x)
    df_files <- read.fwf(file = x,
                         widths = c(10, 5, 4, 10, 14, 13, 60),
                         header = FALSE,
                         stringsAsFactors = FALSE)

    colnames(df_files) <- c("perm", "unknown1", "unknown2", "unknown3", "size", "date", "file")

    dt_files <- as.data.table(df_files)
    dt_files[, time_or_year := substr(date, nchar(date) - 4, nchar(date))]

    current_year <- as.integer(format(Sys.Date(), "%Y"))
    dt_files[, time_and_year := ifelse(grepl(" [0-9]{4}$", time_or_year),
                                       time_or_year,
                                       paste(current_year, time_or_year))]

    dt_files[, date := as.Date(paste(format(Sys.Date(), "%Y"), date), format = "%Y %b %d %H:%M")]

    dt_files <- dt_files[unknown1 == 1]
    return(dt_files)
}



parse_files <- function(x) {
    lines <- strsplit(x, "\n")[[1]]

    parsed <- lapply(lines, function(line) {
        list(
            perm = regmatches(line, regexpr("^[a-z-]+", line)),
            dir = regmatches(line, regexpr("[0-9][[:space:]][a-z]+", line)),
            size = regmatches(line, regexec("[0-9]{2,}", line))[[1]][2],
            date = regmatches(line, regexpr("[A-Za-z]{3}\\s+[0-9]{1,2}\\s+([0-9]{2}:[0-9]{2}|[0-9]{4})", line)),
            file = regmatches(line, regexpr("[A-Za-z0-9_.-]+$", line))
        )
    })

    parsed <- lapply(parsed, function(row) {
        row[vapply(row, length, 1) == 0] <- ""
        row
    })

    dt_parsed <- rbindlist(parsed, fill = TRUE)
    return(dt_parsed)
}


strex <- function(string, pattern) {
    regmatches(string, regexpr(pattern, string))
}

strexg <- function(string, pattern) {
    regmatches(string, gregexpr(pattern, string))
}
