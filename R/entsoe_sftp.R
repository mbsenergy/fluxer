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
