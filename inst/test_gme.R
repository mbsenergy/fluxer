box::use(data.table[...])
box::use(magrittr[...])
box::use(curl[...])
box::use(xml2[...])
# box::use(fluxer[...])
devtools::load_all()

# MGP Price -----------------

## 1.1 Parameters ------------
n <- 5

data_type <- 'MGP_Prezzi'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 1.2 Get files available at GME folder ------------
mgp_price_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_price_files, n)
print(last_n_files)

## 1.3 Download DATASET CLEAN------------

list_mgp_prices <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        mgp_download_file(
            filename = file,
            data_type = 'MGP_Prezzi',
            output_dir = output_dir,
            username = username,       # FTP username for authentication
            password = password,       # FTP password for authentication
            raw = FALSE
        )
    }, error = function(e) {
        # In case of an error (e.g., failed download or processing), return NULL
        message("Error processing file: ", file, " - ", e$message)
        return(NULL)
    })
})

dt_mgp_prices = rbindlist(list_mgp_prices)
print(dt_mgp_prices)

## 1.4 Download RAW DATA------------

lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        mgp_download_file(
            filename = file,
            data_type = data_type,
            output_dir = output_dir,
            username = username,       # FTP username for authentication
            password = password,       # FTP password for authentication
            raw = TRUE
        )
    }, error = function(e) {
        # In case of an error (e.g., failed download or processing), return NULL
        message("Error processing file: ", file, " - ", e$message)
        return(NULL)
    })
})



# MGP Quantità -----------------

## 2.1 Parameters ------------
n <- 5

data_type <- 'MGP_Quantita'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 2.2 Get files available at GME folder ------------
mgp_quantita_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_quantita_files, n)
print(last_n_files)

## 2.3 Download DATASET CLEAN------------

list_mgp_quantita <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        mgp_download_file(
            filename = file,
            data_type = data_type,
            output_dir = output_dir,
            username = username,       # FTP username for authentication
            password = password,       # FTP password for authentication
            raw = FALSE
        )
    }, error = function(e) {
        # In case of an error (e.g., failed download or processing), return NULL
        message("Error processing file: ", file, " - ", e$message)
        return(NULL)
    })
})

dt_mgp_quantita = rbindlist(list_mgp_quantita)
print(dt_mgp_quantita)

## 2.4 Download RAW DATA------------

lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        mgp_download_file(
            filename = file,
            data_type = data_type,
            output_dir = output_dir,
            username = username,       # FTP username for authentication
            password = password,       # FTP password for authentication
            raw = TRUE
        )
    }, error = function(e) {
        # In case of an error (e.g., failed download or processing), return NULL
        message("Error processing file: ", file, " - ", e$message)
        return(NULL)
    })
})
