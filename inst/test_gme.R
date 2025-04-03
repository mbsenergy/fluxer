
# SETUP -----------------

box::use(data.table[...])
box::use(magrittr[...])
box::use(curl[...])
box::use(xml2[...])
# box::use(fluxer[...])
devtools::load_all()



# PRICES:  MGP Price -----------------

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

dt_mgp_prices = rbindlist(list_mgp_prices)
print(dt_mgp_prices)

## 1.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MGP Quantità -----------------

## 2.1 Parameters ------------

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

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MGP Fabbisogno -----------------

## 3.1 Parameters ------------

data_type <- 'MGP_Fabbisogno'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 3.2 Get files available at GME folder ------------
mgp_fabb_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_fabb_files, n)
print(last_n_files)

## 3.3 Download DATASET CLEAN------------

list_mgp_fabb <- lapply(last_n_files, function(file) {
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

dt_mgp_fabb = rbindlist(list_mgp_fabb)
print(dt_mgp_fabb)

## 3.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })




# PRICES:  MGP Liquidita -----------------

## 4.1 Parameters ------------

data_type <- 'MGP_Liquidita'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 4.2 Get files available at GME folder ------------
mgp_liq_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_liq_files, n)
print(last_n_files)

## 4.3 Download DATASET CLEAN------------

list_mgp_liq <- lapply(last_n_files, function(file) {
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

dt_mgp_liq = rbindlist(list_mgp_liq)
print(dt_mgp_liq)

## 4.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MGP Transiti -----------------

## 5.1 Parameters ------------

data_type <- 'MGP_Transiti'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 5.2 Get files available at GME folder ------------
mgp_tran_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_tran_files, n)
print(last_n_files)

## 5.3 Download DATASET CLEAN------------

list_mgp_tran <- lapply(last_n_files, function(file) {
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

dt_mgp_tran = rbindlist(list_mgp_tran)
print(dt_mgp_tran)

## 5.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MGP Limiti Transiti -----------------

## 6.1 Parameters ------------

data_type <- 'MGP_LimitiTransito'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 6.2 Get files available at GME folder ------------
mgp_limtran_files = gme_mgp_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mgp_limtran_files, n)
print(last_n_files)

## 6.3 Download DATASET CLEAN------------

list_mgp_limtran <- lapply(last_n_files, function(file) {
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

dt_mgp_limtran = rbindlist(list_mgp_limtran)
print(dt_mgp_limtran)

## 6.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         mgp_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MSD -----------------

## 7.1 Parameters ------------

data_type <- 'MSD_ServiziDispacciamento'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 7.2 Get files available at GME folder ------------
msd_all_files = gme_rest_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(msd_all_files, n)
print(last_n_files)

## 7.3 Download DATASET CLEAN------------

list_msd_all <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_other_download_file(
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

dt_msd_all = rbindlist(list_msd_all)
print(dt_msd_all)

## 7.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         gme_other_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES:  MB RS -----------------

## 8.1 Parameters ------------

data_type <- 'MB_PRiservaSecondaria'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 8.2 Get files available at GME folder ------------
mb_rs_files = gme_rest_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mb_rs_files, n)
print(last_n_files)

## 8.3 Download DATASET CLEAN------------

list_mb_rs <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_other_download_file(
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

dt_mb_rs = rbindlist(list_mb_rs)
print(dt_mb_rs)

## 8.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         gme_other_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })


# PRICES: MB AS -----------------

## 9.1 Parameters ------------

data_type <- 'MB_PAltriServizi'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 9.2 Get files available at GME folder ------------
mb_as_files = gme_rest_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mb_as_files, n)
print(last_n_files)

## 9.3 Download DATASET CLEAN------------

list_mb_as <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_other_download_file(
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

dt_mb_as = rbindlist(list_mb_as)
print(dt_mb_as)

## 9.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         gme_other_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })


# PRICES: MB TOTALI -----------------

## 10.1 Parameters ------------

data_type <- 'MB_PTotali'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 10.2 Get files available at GME folder ------------
mb_tl_files = gme_rest_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(mb_tl_files, n)
print(last_n_files)

## 10.3 Download DATASET CLEAN------------

list_mb_tl <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_other_download_file(
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

dt_mb_tl = rbindlist(list_mb_tl)
print(dt_mb_tl)

## 10.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         gme_other_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# PRICES: XBID -----------------

## 11.1 Parameters ------------

data_type <- 'XBID_EsitiTotali'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 11.2 Get files available at GME folder ------------
xbid_all_files = gme_rest_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(xbid_all_files, n)
print(last_n_files)

## 11.3 Download DATASET CLEAN------------

list_xbid_all <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_other_download_file(
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

dt_xbid_all = rbindlist(list_xbid_all)
print(dt_xbid_all)

## 11.4 Download RAW DATA------------

# lapply(last_n_files, function(file) {
#     tryCatch({
#         # Call mgp_download_file with explicit arguments
#         gme_other_download_file(
#             filename = file,
#             data_type = data_type,
#             output_dir = output_dir,
#             username = username,       # FTP username for authentication
#             password = password,       # FTP password for authentication
#             raw = TRUE
#         )
#     }, error = function(e) {
#         # In case of an error (e.g., failed download or processing), return NULL
#         message("Error processing file: ", file, " - ", e$message)
#         return(NULL)
#     })
# })



# OFFERS: MGP -----------------

## 1.1 Parameters ------------
n <- 2

data_type <- 'MGP'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "inst/data"


## 1.2 Get files available at GME folder ------------
gme_mgp_offers_files = gme_offers_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(gme_mgp_offers_files, n)
print(last_n_files)


## 1.3 Download DATASET CLEAN------------

list_mgp_offers <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_download_offers_file(
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

dt_mgp_offers = rbindlist(list_mgp_offers, fill = TRUE)
print(dt_mgp_offers)



# OFFERS: MSD -----------------

## 2.1 Parameters ------------

data_type <- 'MSD'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "inst/data"


## 2.2 Get files available at GME folder ------------
gme_msd_offers_files = gme_offers_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(gme_msd_offers_files, n)
print(last_n_files)


## 2.3 Download DATASET CLEAN------------

list_msd_offers <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_download_offers_file(
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

dt_msd_offers = rbindlist(list_msd_offers)
print(dt_msd_offers)



# OFFERS: MB -----------------

## 3.1 Parameters ------------

data_type <- 'MB'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "inst/data"


## 3.2 Get files available at GME folder ------------
gme_mb_offers_files = gme_offers_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(gme_mb_offers_files, n)
print(last_n_files)


## 3.3 Download DATASET CLEAN------------

list_mb_offers <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_download_offers_file(
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

dt_mb_offers = rbindlist(list_mb_offers)
print(dt_mb_offers)



# OFFERS: XBID -----------------

## 4.1 Parameters ------------

data_type <- 'XBID'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "inst/data"


## 4.2 Get files available at GME folder ------------
gme_xbid_offers_files = gme_offers_get_files(data_type = data_type, output_dir = output_dir, username = username, password = password)

last_n_files <- tail(gme_xbid_offers_files, n)
print(last_n_files)


## 4.3 Download DATASET CLEAN------------

list_xbid_offers <- lapply(last_n_files, function(file) {
    tryCatch({
        # Call mgp_download_file with explicit arguments
        gme_download_offers_file(
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

dt_xbid_offers = rbindlist(list_xbid_offers)
print(dt_xbid_offers)

