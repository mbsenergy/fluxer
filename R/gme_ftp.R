#' Retrieve Files from GME FTP Server
#'
#' This function connects to the GME FTP server to list XML files available in a specified directory.
#' It can filter files based on the provided `data_type`.
#'
#' @param data_type A string specifying the data type directory to query.
#'        Default is "MGP_Prezzi". Can be customized for other directories.
#' @param output_dir A string specifying the directory where output files will be saved.
#'        Default is "data". If the directory does not exist, it will be created.
#' @param username FTP server username. Default is "PIASARACENO".
#' @param password FTP server password. Default is "18N15C9R".
#' @param verbose Logical. If TRUE, prints the list of available files. Default is FALSE.
#'
#' @return A character vector containing the filenames that match the specified pattern,
#'         or NULL if an error occurs.
#' @examples
#' \dontrun{
#' files <- gme_mgp_get_files(data_type = "MGP_Prezzi", verbose = TRUE)
#' }
#' @export
gme_mgp_get_files <- function(data_type, output_dir = "data",
                              username = "PIASARACENO", password = "18N15C9R",
                              verbose = FALSE) {

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', data_type, '/')

    if (data_type == "MGP_Prezzi") {file_pattern = "\\S+MGPPrezzi\\.xml$"}
    if (data_type == "MGP_Quantita") {file_pattern = "\\S+MGPQuantita\\.xml$"}

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # List files on the FTP server
    tryCatch({
        ftp_list_cmd <- paste0("curl -u ", username, ":", password, " ", url_base)
        file_list <- system(ftp_list_cmd, intern = TRUE)
        matches <- regexpr(file_pattern, file_list)
        files <- regmatches(file_list, matches)
        files <- files[nzchar(files)]

        # Print available files
        if(isTRUE(verbose)) {
            message("Available files:")
            print(files)
        }
        message("[OK] Available files download.")
        return(files)
    }, error = function(e) {
        message("[ERROR] Error downloading filenames from GME directory", e$message)
        return(NULL)
    })
    return(files)
}



#' Download and Process MGP Data File
#'
#' This function downloads a file from an FTP server using provided credentials, processes the
#' XML data from the downloaded file, and returns the processed data. After processing, the
#' downloaded XML file is deleted.
#'
#' @param filename A character string representing the name of the file to be downloaded.
#' @param username A character string representing the FTP username for authentication.
#' @param password A character string representing the FTP password for authentication.
#' @param output_dir A character string representing the directory where the downloaded file will be saved.
#'
#' @return A data frame (or `NULL` in case of an error). The data frame contains the processed data
#'         obtained from the downloaded XML file, or `NULL` if an error occurred during download or processing.
#'
#' @details
#' This function uses the `curl` package to download the file from an FTP server with the provided
#' credentials. After downloading, it passes the downloaded file to the `gme_dam_xml_to_data` function
#' for processing. Finally, the downloaded XML file is deleted from the local system to clean up.
#'
#' @examples
#' # Example usage:
#' result <- mgp_download_file("example.xml", "your_username", "your_password", "/path/to/output_dir")
#' print(result)
#'
#' @import curl
#' @import data.table
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
mgp_download_file <- function(filename, data_type = 'MGP_Prezzi', output_dir, username, password, raw = FALSE) {
    # Construct the file URL

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', data_type, '/')
    file_url <- paste0(url_base, filename)

    # Construct the output file path
    output_file <- file.path(output_dir, filename)

    # Create a curl handle
    h <- curl::new_handle()
    curl::handle_setopt(h,
                        .list = list(
                            userpwd = paste0(username, ":", password),
                            ftp_use_epsv = TRUE))  # Use passive mode for FTP

    # Perform the download and process the XML file
    result_df <- tryCatch({
        curl::curl_download(file_url, output_file, handle = h)

        # Check if the download was successful
        message("File downloaded successfully: ", output_file)

        # Process the downloaded XML file
        if (isFALSE(raw)) {
            if (data_type == 'MGP_Prezzi') {
                result_df <- gme_dam_price_xml_to_data(output_file)
            }
            # Optionally remove the downloaded file after processing
        } else {
            result_df <- FALSE
        }

        result_df
    }, error = function(e) {
        # Handle errors (e.g., failed download or XML processing)
        message("Error downloading file: ", filename, " - ", e$message)
        result_df = NULL
        result_df
    })

    if (is.null(result_df)) {
        message("An error occurred; result_df is NULL.")
    } else {
        message("Processing completed successfully.")
    }

    if(isFALSE(raw)) {
        file.remove(output_file)
        return(result_df)
    } else {
        message(paste("XML File at:", output_file))
        return(TRUE)
    }
}


# Define the function
#' Process XML data and return a data frame in long format
#'
#' This function reads an XML file containing pricing data, extracts relevant fields
#' from the XML structure, reshapes the data into a long format, and returns the data
#' as a data frame.
#'
#' @param xml_file_path A string representing the path to the XML file to be processed.
#'
#' @return A data frame in long format containing the following columns:
#'   - `DATE`: The date of the data.
#'   - `MARKET`: The market type (e.g., "PUN", "NAT").
#'   - `HOUR`: The time (hour) of the data.
#'   - `ZONE`: The zone of the data (e.g., "CNOR", "CSUD").
#'   - `VALUE`: The corresponding value for the zone (numeric).
#'   - `UNIT`: A constant value for the unit, which is always "EUR".
#'
#' @examples
#' # Specify the XML file path (update with actual file path)
#' xml_file_path <- "path/to/your/xml/file.xml"
#'
#' # Call the function and store the result in a data frame
#' result_df <- process_xml_to_data(xml_file_path)
#'
#' # Print the resulting data frame
#' print(result_df)
#' @export
#' @noRd

gme_dam_price_xml_to_data <- function(xml_file_path) {

    xml_data <- read_xml(xml_file_path)

    prezzi_nodes <- xml_find_all(xml_data, ".//Prezzi")

    # Extract the data from each 'Prezzi' element
    data_list <- lapply(prezzi_nodes, function(node) {
        data <- xml_text(xml_find_first(node, ".//Data"))
        mercato <- xml_text(xml_find_first(node, ".//Mercato"))
        ora <- xml_text(xml_find_first(node, ".//Ora"))
        pun <- xml_text(xml_find_first(node, ".//PUN"))
        nat <- xml_text(xml_find_first(node, ".//NAT"))
        cala <- xml_text(xml_find_first(node, ".//CALA"))
        cnor <- xml_text(xml_find_first(node, ".//CNOR"))
        csud <- xml_text(xml_find_first(node, ".//CSUD"))
        nord <- xml_text(xml_find_first(node, ".//NORD"))
        sard <- xml_text(xml_find_first(node, ".//SARD"))
        sici <- xml_text(xml_find_first(node, ".//SICI"))
        sud <- xml_text(xml_find_first(node, ".//SUD"))
        aust <- xml_text(xml_find_first(node, ".//AUST"))
        coac <- xml_text(xml_find_first(node, ".//COAC"))
        coup <- xml_text(xml_find_first(node, ".//COUP"))
        cors <- xml_text(xml_find_first(node, ".//CORS"))
        fran <- xml_text(xml_find_first(node, ".//FRAN"))
        grec <- xml_text(xml_find_first(node, ".//GREC"))
        slov <- xml_text(xml_find_first(node, ".//SLOV"))
        sviz <- xml_text(xml_find_first(node, ".//SVIZ"))
        bsp <- xml_text(xml_find_first(node, ".//BSP"))
        malt <- xml_text(xml_find_first(node, ".//MALT"))
        xaus <- xml_text(xml_find_first(node, ".//XAUS"))
        xfra <- xml_text(xml_find_first(node, ".//XFRA"))
        mont <- xml_text(xml_find_first(node, ".//MONT"))
        xgre <- xml_text(xml_find_first(node, ".//XGRE"))

        # Return the extracted data as a named list
        list(
            Data = data, Mercato = mercato, Ora = ora, PUN = pun, NAT = nat, CALA = cala,
            CNOR = cnor, CSUD = csud, NORD = nord, SARD = sard, SICI = sici, SUD = sud,
            AUST = aust, COAC = coac, COUP = coup, CORS = cors, FRAN = fran, GREC = grec,
            SLOV = slov, SVIZ = sviz, BSP = bsp, MALT = malt, XAUS = xaus, XFRA = xfra,
            MONT = mont, XGRE = xgre
        )
    })

    # Convert the list into a data frame
    data_df <- do.call(rbind, lapply(data_list, as.data.table))

    data_df[, Data := as.Date(Data, format = "%Y%m%d")]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'HOUR', 'MARKET'), variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'EUR']

    return(data_df_lg)
}

