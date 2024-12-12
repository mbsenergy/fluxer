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
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Quantita') {
                result_df <- gme_dam_quantity_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Fabbisogno') {
                result_df <- gme_dam_fabb_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Liquidita') {
                result_df <- gme_dam_liq_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_Transiti') {
                result_df <- gme_dam_tran_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VALUE', 'UNIT'))
            }
            if (data_type == 'MGP_LimitiTransito') {
                result_df <- gme_dam_limtran_xml_to_data(output_file)
                setcolorder(result_df, c('DATE', 'HOUR', 'MARKET', 'ZONE', 'VARIABLE', 'VALUE', 'UNIT'))
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
                         variable.name = "ZONE", value.name = "VALUE")

    # Convert DATE and HOUR to proper formats
    data_dt_long[, DATE := as.Date(DATE, format = "%Y%m%d")]
    data_dt_long[, HOUR := as.integer(HOUR)]
    data_dt_long[, VALUE := as.numeric(VALUE)]

    # Add unit for the values
    data_dt_long[, UNIT := "MWh"]

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
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'HOUR', 'MARKET'), variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'MWh']

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
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))
    data_df_lg <- melt(data_df, id.vars = c('DATE', 'HOUR', 'MARKET'), variable.name = 'ZONE', value.name = 'VALUE')
    data_df_lg[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    data_df_lg[, UNIT := 'MWh']

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
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))

    melted_data = melt(
        data_df,
        id.vars = c("DATE", "MARKET", "HOUR", 'VALUE'),
        measure.vars = c("Da", "A"),
        variable.name = "Type",
        value.name = "ZONE"
    )
    # Rename TransitoMWh to VALUE and add it to the melted table
    melted_data[, Type := NULL]
    melted_data[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    melted_data[, UNIT := 'MWh']

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
    setnames(data_df, c('Data', 'Mercato', 'Ora'), c('DATE', 'MARKET', 'HOUR'))

    melted_data = melt(
        data_df,
        id.vars = c("DATE", "MARKET", "HOUR", "Limite", "Coefficiente"),
        measure.vars = c("Da", "A"),
        variable.name = "Type",
        value.name = "ZONE"
    )
    # Rename TransitoMWh to VALUE and add it to the melted table
    melted_data[, Type := NULL]
    melted_data = melt(
        melted_data,
        id.vars = c("DATE", "MARKET", "HOUR", "ZONE"),
        measure.vars = c("Limite", "Coefficiente"),
        variable.name = "VARIABLE",
        value.name = "VALUE"
    )

    melted_data[, VALUE := as.numeric(gsub(",", ".", VALUE))]
    melted_data[, VARIABLE := as.character(VARIABLE)]
    melted_data[, UNIT := 'MW']

    return(melted_data)
}
