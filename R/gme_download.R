
# GME Directory -----------------------------------------------------------------------------------------------

#' Retrieve FTP Directory Listing from GME
#'
#' This function connects to the GME FTP server and retrieves a directory listing.
#'
#' @param username FTP username. Default is `"PIASARACENO"`.
#' @param password FTP password. Default is `"18N15C9R"`.
#' @return A `data.table` containing the directory listing.
#' @examples
#' \dontrun{
#' file_dt = gme_get_directory()
#' print(file_dt)
#' }
#' @export
gme_get_directory <- function(username = "PIASARACENO", password = "18N15C9R") {

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/')
    ftp_list_cmd = paste0("curl -u ", username, ":", password, " ", url_base)
    file_list = system(ftp_list_cmd, intern = TRUE)

    file_dt = fread(
        text = file_list,
        fill = TRUE,
        header = FALSE
    )

    return(file_dt)
}


# MGP ------------------------------------------------------------------------------------------------------

#' Retrieve Files from GME FTP Server for DAM market
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

    allowed_data_types = c("MGP_Prezzi", "MGP_Quantita", "MGP_Fabbisogno",
                           "MGP_Liquidita", "MGP_Transiti", "MGP_LimitiTransito")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', data_type, '/')

    if (data_type == "MGP_Prezzi") {file_pattern = "\\S+MGPPrezzi\\.xml$"}
    if (data_type == "MGP_Quantita") {file_pattern = "\\S+MGPQuantita\\.xml$"}
    if (data_type == "MGP_Fabbisogno") {file_pattern = "\\S+MGPFabbisogno\\.xml$"}
    if (data_type == "MGP_Liquidita") {file_pattern = "\\S+MGPLiquidita\\.xml$"}
    if (data_type == "MGP_Transiti") {file_pattern = "\\S+MGPTransiti\\.xml$"}
    if (data_type == "MGP_LimitiTransito") {file_pattern = "\\S+MGPLimitiTransito\\.xml$"}

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
            message(crayon::green("Available files:"))
            print(files)
        }
        message(crayon::green("[OK] Available files download."))
        return(files)
    }, error = function(e) {
        message(crayon::red("[ERROR] Error downloading filenames from GME directory", e$message))
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

    allowed_data_types = c("MGP_Prezzi", "MGP_Quantita", "MGP_Fabbisogno",
                           "MGP_Liquidita", "MGP_Transiti", "MGP_LimitiTransito")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    # Validate filename
    validated_filename = gme_validate_filename(filename = filename, num_digits = 8, file_extension = "xml")
    if(isTRUE(validated_filename)) {

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
        message(crayon::green("File downloaded successfully: ", output_file))

        # Process the downloaded XML file
        if (isFALSE(raw)) {
            if (data_type == 'MGP_Prezzi') {
                result_df <- gme_dam_price_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Quantita') {
                result_df <- gme_dam_quantity_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Fabbisogno') {
                result_df <- gme_dam_fabb_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Liquidita') {
                result_df <- gme_dam_liq_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Transiti') {
                result_df <- gme_dam_tran_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_LimitiTransito') {
                result_df <- gme_dam_limtran_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'VALUE', 'UNIT'))
            }
            # Optionally remove the downloaded file after processing
        } else {
            result_df <- FALSE
        }

        result_df
    }, error = function(e) {
        # Handle errors (e.g., failed download or XML processing)
        message(crayon::red("[ERROR] Error downloading file: ", filename, " - ", e$message))
        result_df = NULL
        result_df
    })

    if (is.null(result_df)) {
        message(crayon::red("[ERROR] An error occurred; result_df is NULL."))
    } else {
        message(crayon::green("[OK] Processing completed successfully."))
    }

    if(isFALSE(raw)) {
        file.remove(output_file)
        return(result_df)
    } else {
        message(paste(crayon::green("[OK] XML File at:", output_file)))
        return(TRUE)
    }

    } else {
        message(crayon::red("[ERROR] Wrong Filename"))
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
    data_df[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'TIME', 'HOUR', 'MARKET'), variable.factor = FALSE, variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'EUR']

    data_df_lg = unique(data_df_lg)

    return(data_df_lg)
}


#' Process GME DAM Quantity XML Data
#'
#' This function processes a GME Day-Ahead Market (DAM) quantity XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MGP`.
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `Liquidita` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MGPQuantita.xml"
#' quantita_data <- gme_dam_quantity_xml_to_data(file_path)
#' head(quantita_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_dam_quantity_xml_to_data <- function(xml_file_path) {

    xml_data <- read_xml(xml_file_path)

    quantita_nodes <- xml_find_all(xml_data, ".//Quantita")

    # Extract the data from each 'Prezzi' element
    data_list <- lapply(quantita_nodes, function(node) {
        data <- xml_text(xml_find_first(node, ".//Data"))
        mercato <- xml_text(xml_find_first(node, ".//Mercato"))
        ora <- xml_text(xml_find_first(node, ".//Ora"))

        # Extract all quantity fields dynamically
        quantity_fields <- xml_children(node)
        quantities <- sapply(quantity_fields, function(child) {
            value <- xml_text(child)
            gsub(",", ".", value)  # Convert commas to dots for numeric conversion
        })

        # Combine extracted fields into a named list
        c(list(Data = data, Mercato = mercato, Ora = ora), as.list(quantities))
    })

    # Convert the list of records into a data.table
    data_dt <- rbindlist(data_list, fill = TRUE)
    data_dt <- data_dt[, -(1:3), with = FALSE]

    # Standardize column names
    quantita_elements <- c('DATE', 'MARKET', 'HOUR',
        "TOTALE_ACQUISTI", "NAT_ACQUISTI", "CALA_ACQUISTI", "CNOR_ACQUISTI",
        "CSUD_ACQUISTI", "NORD_ACQUISTI", "SARD_ACQUISTI", "SICI_ACQUISTI",
        "SUD_ACQUISTI", "AUST_ACQUISTI", "COAC_ACQUISTI", "COUP_ACQUISTI",
        "CORS_ACQUISTI", "FRAN_ACQUISTI", "GREC_ACQUISTI", "SLOV_ACQUISTI",
        "SVIZ_ACQUISTI", "BSP_ACQUISTI", "MALT_ACQUISTI", "XAUS_ACQUISTI",
        "XFRA_ACQUISTI", "MONT_ACQUISTI", "XGRE_ACQUISTI", "TOTALE_VENDITE",
        "NAT_VENDITE", "CALA_VENDITE", "CNOR_VENDITE", "CSUD_VENDITE",
        "NORD_VENDITE", "SARD_VENDITE", "SICI_VENDITE", "SUD_VENDITE",
        "AUST_VENDITE", "COAC_VENDITE", "COUP_VENDITE", "CORS_VENDITE",
        "FRAN_VENDITE", "GREC_VENDITE", "SLOV_VENDITE", "SVIZ_VENDITE",
        "BSP_VENDITE", "MALT_VENDITE", "XAUS_VENDITE", "XFRA_VENDITE",
        "MONT_VENDITE", "XGRE_VENDITE", "TOTITABSP_VENDITE", "TOTITABSP_ACQUISTI"
    )
    setnames(data_dt, names(data_dt), quantita_elements)

    # Reshape the data to long format
    data_dt_long <- melt(data_dt, id.vars = c("DATE", "MARKET", "HOUR"),
                         variable.name = "ZONE", value.name = "VALUE",
                        variable.factor = FALSE)

    # Convert DATE and HOUR to proper formats
    data_dt_long[, DATE := as.Date(DATE, format = "%Y%m%d")]
    data_dt_long[, HOUR := as.integer(HOUR)]
    data_dt_long[, TIME := paste0(sprintf("%02d", as.numeric(HOUR)), ":00")]
    data_dt_long[, VALUE := as.numeric(VALUE)]

    # Add unit for the values
    data_dt_long[, UNIT := "MWh"]

    data_dt_long = unique(data_dt_long)

    return(data_dt_long)
}



#' Process GME DAM Fabbisogno XML Data
#'
#' This function processes a GME Day-Ahead Market (DAM) fabissogno XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MGP`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `Fabbisogno` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MGPFabbisogno.xml"
#' fabb_data <- gme_dam_fabb_xml_to_data(file_path)
#' head(fabb_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_dam_fabb_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Find all 'Fabbisogno' nodes
    fabbisogno_nodes <- xml_find_all(xml_data, ".//Fabbisogno")

    # Extract the data from each 'Fabbisogno' element
    data_list <- lapply(fabbisogno_nodes, function(node) {
        list(
            Data = xml_text(xml_find_first(node, ".//Data")),
            Mercato = xml_text(xml_find_first(node, ".//Mercato")),
            Ora = xml_text(xml_find_first(node, ".//Ora")),
            Italia = xml_text(xml_find_first(node, ".//Italia")),
            CALA = xml_text(xml_find_first(node, ".//CALA")),
            CNOR = xml_text(xml_find_first(node, ".//CNOR")),
            CSUD = xml_text(xml_find_first(node, ".//CSUD")),
            NORD = xml_text(xml_find_first(node, ".//NORD")),
            SARD = xml_text(xml_find_first(node, ".//SARD")),
            SICI = xml_text(xml_find_first(node, ".//SICI")),
            SUD = xml_text(xml_find_first(node, ".//SUD"))
        )
    })

    # Convert the list to a data.table
    data_df <- rbindlist(data_list, fill = TRUE)

    data_df[, Data := as.Date(Data, format = "%Y%m%d")]
    data_df[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'TIME', 'HOUR', 'MARKET'), variable.factor = FALSE, variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'MWh']

    data_df_lg = unique(data_df_lg)

    return(data_df_lg)
}



#' Process GME DAM Liquidity XML Data
#'
#' This function processes a GME Day-Ahead Market (DAM) Liquidity XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MGP`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `Quantita` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MGPLiquidity.xml"
#' liquidity_data <- gme_dam_liq_xml_to_data(file_path)
#' head(liquidity_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_dam_liq_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Find all 'Fabbisogno' nodes
    dati_nodes = xml_find_all(xml_data, ".//DatiLiquidita")

    # Extract data from each node
    data_df = data.table(
        Data = xml_text(xml_find_all(dati_nodes, "./Data")),
        Mercato = xml_text(xml_find_all(dati_nodes, "./Mercato")),
        Ora = as.integer(xml_text(xml_find_all(dati_nodes, "./Ora"))),
        Liquidita = as.numeric(gsub(",", ".", xml_text(xml_find_all(dati_nodes, "./Liquidita"))))
    )

    data_df[, Data := as.Date(Data, format = "%Y%m%d")]
    data_df[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'TIME', 'HOUR', 'MARKET'), variable.factor = FALSE, variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'MWh']

    data_df_lg = unique(data_df_lg)

    return(data_df_lg)
}



#' Process GME DAM Transiti XML Data
#'
#' This function processes a GME Day-Ahead Market (DAM) Transiti XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MGP`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `Transiti` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MGPTransiti.xml"
#' transit_data <- gme_dam_tran_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_dam_tran_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "MgpTransiti" nodes
    transiti_nodes = xml_find_all(xml_data, ".//MgpTransiti")

    # Extract data from each node
    data_df = data.table(
        Data = xml_text(xml_find_all(transiti_nodes, "./Data")),
        Mercato = xml_text(xml_find_all(transiti_nodes, "./Mercato")),
        Ora = as.integer(xml_text(xml_find_all(transiti_nodes, "./Ora"))),
        Da = xml_text(xml_find_all(transiti_nodes, "./Da")),
        A = xml_text(xml_find_all(transiti_nodes, "./A")),
        VALUE = as.numeric(gsub(",", ".", xml_text(xml_find_all(transiti_nodes, "./TransitoMWh"))))
    )

    data_df[, Data := as.Date(Data, format = "%Y%m%d")]
    data_df[, Da := paste0('from_', Da)]
    data_df[, A := paste0('to_', A)]
    data_df[, DAA := paste(Da, A, sep = '-')]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df[, Da := NULL]
    data_df[, A := NULL]

    melted_data = melt(
        data_df,
        id.vars = c("DATE", "MARKET", "HOUR", 'VALUE'),
        measure.vars = c('DAA'),
        variable.factor = FALSE,
        variable.name = "Type",
        value.name = "ZONE"
    )
    # Rename TransitoMWh to VALUE and add it to the melted table
    melted_data[, Type := NULL]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(HOUR)), ":00")]
    melted_data[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    melted_data[, UNIT := 'MWh']

    melted_data = unique(melted_data)

    return(melted_data)
}



#' Process GME DAM Limiti Transiti XML Data
#'
#' This function processes a GME Day-Ahead Market (DAM) Limiti Transiti XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MGP`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `Limiti Transiti` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MGPTransiti.xml"
#' transit_data <- gme_dam_tran_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_dam_limtran_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "LimitiTransito" nodes
    transiti_nodes = xml_find_all(xml_data, ".//LimitiTransito")

    # Extract raw data into a data.table
    data_df = data.table(
        Data = xml_text(xml_find_all(transiti_nodes, "./Data")),
        Mercato = xml_text(xml_find_all(transiti_nodes, "./Mercato")),
        Ora = as.integer(xml_text(xml_find_all(transiti_nodes, "./Ora"))),
        Da = xml_text(xml_find_all(transiti_nodes, "./Da")),
        A = xml_text(xml_find_all(transiti_nodes, "./A")),
        Limite = as.numeric(gsub(",", ".", xml_text(xml_find_all(transiti_nodes, "./Limite")))),
        Coefficiente = as.numeric(gsub(",", ".", xml_text(xml_find_all(transiti_nodes, "./Coefficiente"))))
    )

    data_df[, Data := as.Date(Data, format = "%Y%m%d")]
    data_df[, Da := paste0('from_', Da)]
    data_df[, A := paste0('to_', A)]
    data_df[, DAA := paste(Da, A, sep = '-')]
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df[, Da := NULL]
    data_df[, A := NULL]

    melted_data = melt(
        data_df,
        id.vars = c("DATE", "MARKET", "HOUR", "Limite", "Coefficiente"),
        measure.vars = c("DAA"),
        variable.factor = FALSE,
        variable.name = "Type",
        value.name = "ZONE"
    )
    # Rename TransitoMWh to VALUE and add it to the melted table
    melted_data[, Type := NULL]
    melted_data = melt(
        melted_data,
        id.vars = c("DATE", "MARKET", "HOUR", "ZONE"),
        measure.vars = c("Limite", "Coefficiente"),
        variable.factor = FALSE,
        variable.name = "VARIABLE",
        value.name = "VALUE"
    )

    melted_data[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(HOUR)), ":00")]
    melted_data[, VARIABLE := as.character(VARIABLE)]
    melted_data[, UNIT := 'MW']

    melted_data = unique(melted_data)

    return(melted_data)
}



# OTHER MARKETS ------------------------------------------------------------------------------------------------------

## MSD ------------------------------------------------------------------------------------------------------


#' Retrieve Files from GME FTP Server for Other markets
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
gme_rest_get_files <- function(data_type, output_dir = "data",
                              username = "PIASARACENO", password = "18N15C9R",
                              verbose = FALSE) {

    # Define allowed data types
    allowed_data_types = c("MSD_ServiziDispacciamento", "MB_PRiservaSecondaria",
                           "MB_PAltriServizi", "MB_PTotali", "XBID_EsitiTotali")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', data_type, '/')

    if (data_type == "MSD_ServiziDispacciamento") {file_pattern = "\\S+MSDServiziDispacciamento\\.xml$"}
    if (data_type == "MB_PRiservaSecondaria") {file_pattern = "\\S+MBPRiservaSecondaria\\.xml$"}
    if (data_type == "MB_PAltriServizi") {file_pattern = "\\S+MBPAltriServizi\\.xml$"}
    if (data_type == "MB_PTotali") {file_pattern = "\\S+MBPTotali\\.xml$"}
    if (data_type == "XBID_EsitiTotali") {file_pattern = "\\S+XBIDEsitiTotali\\.xml$"}

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
            message(crayon::green("[OK] Available files:"))
            print(files)
        }
        message(crayon::green("[OK] Available files download."))
        return(files)
    }, error = function(e) {
        message(crayon::red("[ERROR] Error downloading filenames from GME directory", e$message))
        return(NULL)
    })
    return(files)
}



#' Download and Process Other Markets Data File
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
#' result <- gmeother_download_file("example.xml", "your_username", "your_password", "/path/to/output_dir")
#' print(result)
#'
#' @import curl
#' @import data.table
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @export
gme_other_download_file <- function(filename, data_type = 'MSD_ServiziDispacciamento', output_dir, username, password, raw = FALSE) {
    # Construct the file URL

    # Define allowed data types
    allowed_data_types = c("MSD_ServiziDispacciamento", "MB_PRiservaSecondaria",
                           "MB_PAltriServizi", "MB_PTotali", "XBID_EsitiTotali")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    # Validate filename
    validated_filename = gme_validate_filename(filename = filename, num_digits = 8, file_extension = "xml")
    if(isTRUE(validated_filename)) {

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
        message(crayon::green("File downloaded successfully: ", output_file))


        # Process the downloaded XML file
        if (isFALSE(raw)) {
            if (data_type == 'MSD_ServiziDispacciamento') {
                result_df <- gme_msd_all_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MB_PRiservaSecondaria') {
                result_df <- gme_mb_rs_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'FIELD', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MB_PAltriServizi') {
                result_df <- gme_mb_as_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'FIELD', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MB_PTotali') {
                result_df <- gme_mb_tl_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'FIELD', 'VALUE', 'UNIT'))
            }
            if (data_type == 'XBID_EsitiTotali') {
                result_df <- gme_xbid_all_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'TIME', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'VALUE', 'UNIT'))
            }
            # Optionally remove the downloaded file after processing
        } else {
            result_df <- FALSE
        }

        result_df
    }, error = function(e) {
        # Handle errors (e.g., failed download or XML processing)
        message(crayon::red("[ERROR] Error downloading file: ", filename, " - ", e$message))
        result_df = NULL
        result_df
    })

    if (is.null(result_df)) {
        message(crayon::red("[ERROR] An error occurred; result_df is NULL."))
    } else {
        message(crayon::green("[OK] Processing completed successfully."))
    }

    if(isFALSE(raw)) {
        file.remove(output_file)
        return(result_df)
    } else {
        message(paste(crayon::green("[OK] XML File at:", output_file)))
        return(TRUE)
    }

    } else {
        message(crayon::red("[ERROR] Wrong Filename"))
    }

}



#' Process GME Other Markets MSD XML Data
#'
#' This function processes a GME Other Markets MSD XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MSD`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `MSD` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MSD_ServiziDispacciamento.xml"
#' transit_data <- gme_msd_all_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_msd_all_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "ServiziDispacciamento" nodes
    sd_nodes <- xml_find_all(xml_data, ".//ServiziDispacciamento")

    # Initialize an empty list to store intermediate data.tables
    data_list <- lapply(sd_nodes, function(node) {
        # Extract each child node within "ServiziDispacciamento"
        child_nodes <- xml_children(node)

        # Extract field names and values
        field_names <- xml_name(child_nodes)
        field_values <- xml_text(child_nodes)

        # Replace commas with dots for numeric conversion and create a data.table
        dt <- data.table::data.table(
            FIELD = field_names,
            VALUE = gsub(",", ".", field_values)
        )
        # Reshape to wide format
        data.table::dcast(dt, . ~ FIELD, value.var = "VALUE")[, . := NULL] # Drop placeholder column '.'
    })

    # Combine all intermediate data.tables
    combined_data <- rbindlist(data_list, fill = TRUE)

    # Clean and type-convert fields where necessary
    combined_data[, `:=`(
        Data = as.Date(Data, format = "%Y%m%d"),
        Ora = as.integer(Ora)
    )]

    # Tidy the data for analysis
    melted_data <- melt(
        combined_data,
        id.vars = c("Data", "Ora", "Mercato"),
        variable.name = "ZONE_VARIABLE",
        variable.factor = FALSE,
        value.name = "VALUE"
    )

    # Separate ZONE and VARIABLE
    melted_data[, `:=`(
        ZONE = ifelse(grepl("_", ZONE_VARIABLE), sub("_.*", "", ZONE_VARIABLE), "TOTAL"),
        VARIABLE = ifelse(grepl("_", ZONE_VARIABLE), sub(".*_", "", ZONE_VARIABLE), ZONE_VARIABLE)
    )]
    # Assign units

    melted_data[, UNIT := ifelse(grepl("MWh", VARIABLE), "MWh", "EUR")]
    melted_data[, ZONE_VARIABLE := NULL]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(melted_data, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))

    return(melted_data)
}


#' Process GME Other Markets MB RS XML Data
#'
#' This function processes a GME Other Markets MB RS XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MB RS`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `MB RS` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MB_PRiservaSecondaria.xml"
#' transit_data <- gme_mb_rs_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_mb_rs_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "ServiziDispacciamento" nodes
    sd_nodes <- xml_find_all(xml_data, ".//ServiziDispacciamento")

    # Initialize an empty list to store intermediate data.tables
    data_list <- lapply(sd_nodes, function(node) {
        # Extract each child node within "ServiziDispacciamento"
        child_nodes <- xml_children(node)

        # Extract field names and values
        field_names <- xml_name(child_nodes)
        field_values <- xml_text(child_nodes)

        # Replace commas with dots for numeric conversion and create a data.table
        dt <- data.table::data.table(
            FIELD = field_names,
            VALUE = gsub(",", ".", field_values)
        )
        # Reshape to wide format
        data.table::dcast(dt, . ~ FIELD, value.var = "VALUE")[, . := NULL] # Drop placeholder column '.'
    })

    # Combine all intermediate data.tables
    combined_data <- rbindlist(data_list, fill = TRUE)

    # Clean and type-convert fields where necessary
    combined_data[, `:=`(
        Data = as.Date(Data, format = "%Y%m%d"),
        Ora = as.integer(Ora),
        Periodo = as.integer(Periodo)
    )]

    # Tidy the data for analysis
    melted_data <- melt(
        combined_data,
        id.vars = c("Data", "Ora", "Mercato", "Periodo"),
        variable.name = "ZONE_VARIABLE",
        variable.factor = FALSE,
        value.name = "VALUE"
    )

    # Separate ZONE and VARIABLE
    melted_data[, `:=`(
        ZONE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, "TOTAL", sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 1)),  # First element or "TOTAL"
        VARIABLE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, ZONE_VARIABLE,
                          ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) >= 2, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 2), NA)),  # Second element or entire value if length is 1
        FIELD = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 3, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 3), NA)  # Third element or NA
    )]
    # Assign units

    melted_data[, UNIT := ifelse(grepl("MWh", VARIABLE), "MWh", "EUR")]
    melted_data[, ZONE_VARIABLE := NULL]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(melted_data, c('Data', 'Mercato', 'Ora', 'Periodo'), c('DATE', 'MARKET', 'HOUR', 'PERIOD'))
    melted_data[, MARKET := 'MB Riserva Secondaria']

    return(melted_data)
}


#' Process GME Other Markets MS AS XML Data
#'
#' This function processes a GME Other Markets MS AS XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MS AS`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `MS AS` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MB_PRiservaSecondaria.xml"
#' transit_data <- gme_mb_rs_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_mb_as_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "ServiziDispacciamento" nodes
    sd_nodes <- xml_find_all(xml_data, ".//ServiziDispacciamento")

    # Initialize an empty list to store intermediate data.tables
    data_list <- lapply(sd_nodes, function(node) {
        # Extract each child node within "ServiziDispacciamento"
        child_nodes <- xml_children(node)

        # Extract field names and values
        field_names <- xml_name(child_nodes)
        field_values <- xml_text(child_nodes)

        # Replace commas with dots for numeric conversion and create a data.table
        dt <- data.table::data.table(
            FIELD = field_names,
            VALUE = gsub(",", ".", field_values)

        )
        # Reshape to wide format
        data.table::dcast(dt, . ~ FIELD, value.var = "VALUE")[, . := NULL] # Drop placeholder column '.'
    })

    # Combine all intermediate data.tables
    combined_data <- rbindlist(data_list, fill = TRUE)

    # Clean and type-convert fields where necessary
    combined_data[, `:=`(
        Data = as.Date(Data, format = "%Y%m%d"),
        Ora = as.integer(Ora),
        Periodo = as.integer(Periodo)
    )]

    # Tidy the data for analysis
    melted_data <- melt(
        combined_data,
        id.vars = c("Data", "Ora", "Mercato", "Periodo"),
        variable.name = "ZONE_VARIABLE",
        variable.factor = FALSE,
        value.name = "VALUE"
    )

    # Separate ZONE and VARIABLE
    melted_data[, `:=`(
        ZONE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, "TOTAL", sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 1)),  # First element or "TOTAL"
        VARIABLE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, ZONE_VARIABLE,
                          ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) >= 2, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 2), NA)),  # Second element or entire value if length is 1
        FIELD = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 3, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 3), NA)  # Third element or NA
    )]
    # Assign units

    melted_data[, UNIT := ifelse(grepl("MWh", VARIABLE), "MWh", "EUR")]
    melted_data[, ZONE_VARIABLE := NULL]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(melted_data, c('Data', 'Mercato', 'Ora', 'Periodo'), c('DATE', 'MARKET', 'HOUR', 'PERIOD'))
    melted_data[, MARKET := 'MB Altri Servizi']

    return(melted_data)
}


#' Process GME Other Markets MB Totali XML Data
#'
#' This function processes a GME Other Markets MB Totali XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `MB Totali`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `MSD` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/MBPTotali.xml"
#' transit_data <- gme_mb_tl_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_mb_tl_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "ServiziDispacciamento" nodes
    sd_nodes <- xml_find_all(xml_data, ".//ServiziDispacciamento")

    # Initialize an empty list to store intermediate data.tables
    data_list <- lapply(sd_nodes, function(node) {
        # Extract each child node within "ServiziDispacciamento"
        child_nodes <- xml_children(node)

        # Extract field names and values
        field_names <- xml_name(child_nodes)
        field_values <- xml_text(child_nodes)

        # Replace commas with dots for numeric conversion and create a data.table
        dt <- data.table::data.table(
            FIELD = field_names,
            VALUE = gsub(",", ".", field_values)
        )
        # Reshape to wide format
        data.table::dcast(dt, . ~ FIELD, value.var = "VALUE")[, . := NULL] # Drop placeholder column '.'
    })

    # Combine all intermediate data.tables
    combined_data <- rbindlist(data_list, fill = TRUE)

    # Clean and type-convert fields where necessary
    combined_data[, `:=`(
        Data = as.Date(Data, format = "%Y%m%d"),
        Ora = as.integer(Ora),
        Periodo = as.integer(Periodo)
    )]

    # Tidy the data for analysis
    melted_data <- melt(
        combined_data,
        id.vars = c("Data", "Ora", "Mercato", "Periodo"),
        variable.name = "ZONE_VARIABLE",
        variable.factor = FALSE,
        value.name = "VALUE"
    )

    # Separate ZONE and VARIABLE
    melted_data[, `:=`(
        ZONE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, "TOTAL", sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 1)),  # First element or "TOTAL"
        VARIABLE = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 1, ZONE_VARIABLE,
                          ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) >= 2, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 2), NA)),  # Second element or entire value if length is 1
        FIELD = ifelse(lengths(strsplit(ZONE_VARIABLE, "_")) == 3, sapply(strsplit(ZONE_VARIABLE, "_"), `[`, 3), NA)  # Third element or NA
    )]
    # Assign units

    melted_data[, UNIT := ifelse(grepl("MWh", VARIABLE), "MWh", "EUR")]
    melted_data[, ZONE_VARIABLE := NULL]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(Ora)), ":00")]
    setnames(melted_data, c('Data', 'Mercato', 'Ora', 'Periodo'), c('DATE', 'MARKET', 'HOUR', 'PERIOD'))
    melted_data[, MARKET := 'MB Totali']

    return(melted_data)
}



#' Process GME Other Markets XBID XML Data
#'
#' This function processes a GME Other Markets MSD XML file and extracts
#' structured data, converting it into a tidy `data.table` format. It dynamically parses
#' all quantity fields, standardizes their names, and reshapes the data into long format.
#'
#' @param xml_file_path A string specifying the file path to the XML file containing quantity data.
#'
#' @return A `data.table` in long format with the following columns:
#' \itemize{
#'   \item `DATE`: Date of the record (as a Date object).
#'   \item `MARKET`: Market type, typically `XBID`.
#'   \item `TIME`: Hour/minutes of the record (datetime).
#'   \item `HOUR`: Hour of the record (integer).
#'   \item `ZONE`: Zone or region (e.g., `CSUD`, `CALA`).
#'   \item `VALUE`: Numeric value of the quantity in MWh.
#'   \item `UNIT`: Unit of the value, default is `MWh`.
#' }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Reads the XML file and locates all `XBID` nodes.
#'   \item Extracts key fields (`Data`, `Mercato`, `Ora`) and dynamically captures all quantity-related fields.
#'   \item Converts commas in values to dots for numeric compatibility.
#'   \item Creates a wide-format `data.table` with standardized column names.
#'   \item Reshapes the data to a long format for better analysis and visualization.
#' }
#'
#' @examples
#' \dontrun{
#' # Process an XML file
#' file_path <- "path/to/XBID.XML"
#' transit_data <- gme_xbid_all_xml_to_data(file_path)
#' head(transit_data)
#' }
#'
#' @import data.table
#' @import xml2
#' @export
gme_xbid_all_xml_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Extract all "Table" nodes
    table_nodes <- xml_find_all(xml_data, ".//Table")

    # Initialize an empty list to store intermediate data.tables
    data_list <- lapply(table_nodes, function(node) {
        # Extract each element within the "Table" node
        flow_date <- xml_text(xml_find_first(node, ".//FlowDate"))
        hour <- xml_text(xml_find_first(node, ".//Hour"))
        zone <- xml_text(xml_find_first(node, ".//Zone"))
        first <- xml_text(xml_find_first(node, ".//First"))
        last <- xml_text(xml_find_first(node, ".//Last"))
        min_value <- xml_text(xml_find_first(node, ".//Min"))
        max_value <- xml_text(xml_find_first(node, ".//Max"))
        riferimento <- xml_text(xml_find_first(node, ".//Riferimento"))
        last_hour <- xml_text(xml_find_first(node, ".//LastHour"))
        acquisti <- xml_text(xml_find_first(node, ".//Acquisti"))
        vendite <- xml_text(xml_find_first(node, ".//Vendite"))

        # Create a data.table for this entry
        dt <- data.table::data.table(
            DATE = flow_date,
            HOUR = hour,
            ZONE = zone,
            First = first,
            Last = last,
            Min = min_value,
            Max = max_value,
            Riferimento = riferimento,
            LastHour = last_hour,
            Acquisti = acquisti,
            Vendite = vendite
        )

        return(dt)
    })

    # Combine all intermediate data.tables
    combined_data <- rbindlist(data_list, fill = TRUE)

    # Clean and type-convert fields where necessary
    combined_data[, `:=`(
        DATE = as.Date(DATE, format = "%Y%m%d"),
        HOUR = as.integer(HOUR),
        First = as.numeric(First),
        Last = as.numeric(Last),
        Min = as.numeric(Min),
        Max = as.numeric(Max),
        Riferimento = as.numeric(Riferimento),
        LastHour = as.numeric(LastHour),
        Acquisti = as.numeric(Acquisti),
        Vendite = as.numeric(Vendite)
    )]

    # Melt the data for analysis (long format)
    melted_data <- melt(
        combined_data,
        id.vars = c("DATE", "HOUR", "ZONE"),
        variable.name = "VARIABLE",
        variable.factor = FALSE,
        value.name = "VALUE"
    )

    # Assign units based on the VARIABLE (for example, "MWh" or "EUR")
    melted_data[, UNIT := ifelse(grepl("MWh", VARIABLE), "MWh", "EUR")]
    melted_data[, TIME := paste0(sprintf("%02d", as.numeric(HOUR)), ":00")]
    melted_data[, MARKET := 'XBID']

    return(melted_data)
}



# PUBBLIC OFFERS ---------------------------------------

#' Retrieve Files from GME FTP Server for DAM market pubblic offers
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
gme_offers_get_files <- function(data_type, output_dir = "data",
                              username = "PIASARACENO", password = "18N15C9R",
                              verbose = FALSE) {

    allowed_data_types = c("MGP", "MSD", "MB",
                           "MI-A1", "MI-A2", "MI-A3", "XBID")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    mkt_data_type = paste0(data_type, '_OffertePubbliche/')

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', mkt_data_type, '/')

    if (data_type == "MGP") {file_pattern = "\\S+MGPOffertePubbliche\\.zip$"}
    if (data_type == "MSD") {file_pattern = "\\S+MSDOffertePubbliche\\.zip$"}
    if (data_type == "MB") {file_pattern = "\\S+MBOffertePubbliche\\.zip$"}
    if (data_type == "MI-A1") {file_pattern = "\\S+MI-A1OffertePubbliche\\.zip$"}
    if (data_type == "MI-A2") {file_pattern = "\\S+MI-A2OffertePubbliche\\.zip$"}
    if (data_type == "MI-A3") {file_pattern = "\\S+MI-A3OffertePubbliche\\.zip$"}
    if (data_type == "XBID") {file_pattern = "\\S+XBIDOffertePubbliche\\.zip$"}

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
            message(crayon::green("Available files:"))
            print(files)
        }
        message(crayon::green("[OK] Available files download."))
        return(files)
    }, error = function(e) {
        message(crayon::red("[ERROR] Error downloading filenames from GME directory", e$message))
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
gme_download_offers_file <- function(filename, data_type = 'MGP', output_dir, username, password, raw = FALSE) {

    # List of allowed data types
    allowed_data_types <- c("MGP", "MSD", "MB", "MI-A1", "MI-A2", "MI-A3", "XBID")

    # Validate the input data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type, ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    # Validate filename
    validated_filename = gme_validate_filename(filename = filename, num_digits = 8, file_extension = "zip")
    if(isTRUE(validated_filename)) {

    # Construct the file URL
    mkt_data_type <- paste0(data_type, '_OffertePubbliche/')
    url_base <- paste0('ftp://download.mercatoelettrico.org/MercatiElettrici/', mkt_data_type)
    file_url <- paste0(url_base, filename)

    # Construct the output file path
    output_file <- file.path(output_dir, filename)

    # Create a curl handle
    h <- curl::new_handle()
    curl::handle_setopt(h, .list = list(
        userpwd = paste0(username, ":", password),
        ftp_use_epsv = TRUE  # Use passive mode for FTP
    ))

    # Attempt to download and process the file
    result_df <- tryCatch({
        # Download the file
        curl::curl_download(file_url, output_file, handle = h)
        message(crayon::green("File downloaded successfully: ", output_file))

        # Check if the downloaded file is a zip
        if (!grepl("\\.zip$", filename)) {
            stop("Expected a .zip file, but got: ", filename)
        }

        # Unzip the file
        files_in_zip <- unzip(output_file, list = TRUE)
        if (nrow(files_in_zip) == 0) {
            stop("The zip file contains no files.")
        }

        # Extract contents and remove the zip
        unzip(output_file, exdir = output_dir)
        file.remove(output_file)

        # Locate the XML file
        xml_files <- list.files(output_dir, pattern = "\\.xml$", full.names = TRUE)
        if (length(xml_files) == 0) {
            stop("No XML files found after extracting the zip.")
        }
        xml_file_name <- xml_files[1]  # Assume the first XML file

        # Process the XML file
        if (!raw) {
                result_df <- gme_offerts_zip_to_data(xml_file_name)
        } else {
            message(crayon::yellow("Raw mode: Skipping processing of XML file."))
            result_df <- NULL
        }

        result_df
    }, error = function(e) {
        # Handle errors
        message(crayon::red("[ERROR] Error occurred: ", e$message))
        NULL
    })

    # Cleanup and return results
    if (!raw && !is.null(result_df)) {
        message(crayon::green("Cleaning up intermediate files."))
        file.remove(xml_file_name)
    } else if (raw) {
        message(paste(crayon::green("[OK] Raw XML file available at:", xml_file_name)))
    }

    return(result_df)

    } else {
        message(crayon::red("[ERROR] Wrong Filename"))
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
#' @return A data frame in long format
#' @examples
#' # Specify the XML file path (update with actual file path)
#' xml_file_path <- "path/to/your/xml/file.xml"
#'
#' # Call the function and store the result in a data frame
#' result_df <- gme_offerts_zip_to_data(xml_file_path)
#'
#' # Print the resulting data frame
#' print(result_df)
#' @export
#' @noRd

gme_offerts_zip_to_data <- function(xml_file_path) {

    # Read the XML file
    xml_data <- read_xml(xml_file_path)

    # Find all "OfferteOperatori" nodes
    offerte_nodes <- xml_find_all(xml_data, ".//OfferteOperatori")

    # Extract data from each "OfferteOperatori" element
    data_list <- lapply(offerte_nodes, function(node) {
        this <- list()
        children <- xml_children(node)

        # Iterate through child elements
        for (child in children) {
            el_name <- xml_name(child)
            val <- xml_text(child)

            # Convert values based on their type
            if (el_name %in% c("INTERVAL_NO", "MERIT_ORDER_NO")) {
                val <- as.integer(val)
            } else if (el_name == "BID_OFFER_DATE_DT") {
                this$bid_offer_date_dt_parsed <- as.Date(val, format = "%Y%m%d")
            } else if (el_name %in% c("QUANTITY_NO", "AWARDED_QUANTITY_NO", "ENERGY_PRICE_NO",
                                      "ADJ_QUANTITY_NO", "AWARDED_PRICE_NO")) {
                val <- as.numeric(val)
            } else if (el_name == "SUBMITTED_DT") {
                # If SUBMITTED_DT is relevant, process it here
                this$submitted_dt <- as.POSIXct(val, format = "%Y%m%d%H%M%OS")
            }

            # Add the value to the list
            this[[tolower(el_name)]] <- val
        }

        # Calculate the time field
        interval_no <- this[["interval_no"]]
        if (!is.null(interval_no) && !is.null(this$bid_offer_date_dt_parsed)) {
            this$time <- as.POSIXct(this$bid_offer_date_dt_parsed) +
                (interval_no * 3600) - ifelse(interval_no != 25, 60 * 60, 90 * 60)
        }

        return(this)
    })

    # Combine all parsed data into a data.table
    data_dt <- rbindlist(data_list, fill = TRUE)

    if ("period" %in% colnames(data_dt)) {
        setnames(data_dt, "period", "interval_no")
    }

    return(data_dt)
}


# IGI ------------------------------------------------------------------------------------------------------

#' Retrieve Files from GME FTP Server for DAM market
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
gme_igi_get_files <- function(data_type, output_dir = "data",
                               username = "PIASARACENO", password = "18N15C9R",
                               verbose = FALSE, storico = FALSE, n = 1) {

    allowed_data_types = c("IGI")

    # Validate data_type
    if (!data_type %in% allowed_data_types) {
        stop(paste("Invalid data_type:", data_type,
                   ". Must be one of:", paste(allowed_data_types, collapse = ", ")))
    }

    # # Validate filename
    # validated_filename = gme_validate_filename(filename = filename, num_digits = 8, file_extension = "xml")
    # if(isTRUE(validated_filename)) {

    url_base = paste0('ftp://download.mercatoelettrico.org/MercatiGas/MGPGAS_IGI//', data_type, '/')

    if (data_type == "IGI") {file_pattern = "\\S+IGI\\.xml$"}

    # Ensure output directory exists
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    # List files and directories on the FTP server
    tryCatch({
        # List files and directories in the specified FTP directory
        ftp_list_cmd <- paste0("curl -u ", username, ":", password, " ftp://download.mercatoelettrico.org/MercatiGas/MGPGAS_IGI/")
        file_list <- system(ftp_list_cmd, intern = TRUE)

        # Print the list of files and directories in the FTP directory
        message(crayon::green("[OK] Listing files and directories on FTP server:"))
        print(file_list)

        # Match files based on the pattern (files that end with 'IGI.xml')
        matches <- regexpr(file_pattern, file_list)
        files <- regmatches(file_list, matches)
        files_to_download <- files[nzchar(files)]  # Filter out empty matches

        if(isTRUE(verbose)) {
            message(crayon::green("[OK] Available files:"))
            print(files_to_download)
        }
    }, error = function(e) {
        message(crayon::red("[ERROR] Error downloading files from FTP server", e$message))
        return(NULL)
    })

    return(files_to_download)

    # } else {
    #     message(crayon::red("[ERROR] Wrong Filename"))
    # }

}


 #' Validate Filename Structure
#'
#' This function checks whether a given filename matches a specific structure based on
#' the number of leading digits, a middle expression, and the file extension.
#'
#' @param filename A string representing the filename to validate.
#' @param num_digits An integer specifying the number of leading digits in the filename. Default is 8.
#' @param middle_expression A string specifying the exact middle expression in the filename. Default is "MGPPrezzi".
#' @param file_extension A string specifying the expected file extension (without the dot). Default is "xml".
#'
#' @return A logical value (`TRUE` if the filename is valid, `FALSE` otherwise).
#'         If the filename is invalid, it prints "Wrong filename" to the console.
#'
#' @examples
#' # Validate a correctly formatted filename
#' gme_validate_filename("20241126MGPPrezzi.xml") # Returns TRUE
#'
#' # Validate an incorrectly formatted filename
#' gme_validate_filename("wrongfile.xml") # Prints "Wrong filename" and returns FALSE
#'
#' # Validate with custom parameters
#' gme_validate_filename("2024MGPPrezzi.txt", num_digits = 4, file_extension = "txt") # Returns TRUE
#'
#' @export
gme_validate_filename = function(filename, num_digits = 8, file_extension = "xml") {
    # Build the regular expression dynamically
    pattern = paste0("^\\d{", num_digits, "}.*\\.", file_extension, "$")

    if (!grepl(pattern, filename)) {
        cat(crayon::red("Wrong filename", "\n"))
        return(FALSE)
    }
    return(TRUE)
}

