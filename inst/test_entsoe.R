
box::use(data.table[...])
box::use(magrittr[...])
box::use(ggplot2[...])
# devtools::load_all()
box::use(fluxer[...])


# 1. API Method ---------------------------------------------

## DAM Prices RAW ----------------------

country <- "Czech Republic"
from_data <- '2024-01-04'
to_data <- '2024-01-06'
# api_key = Sys.getenv('ENTSOE_KEY')
# api_key = ''

dam_prices <- api_entsoe_dam_prices(country, from_data, to_data)
# dam_prices %>% head()
dam_prices

## DAM Prices ----------------------

country <- "Spain"
from_data <- '2024-01-04'
to_data <- '2024-02-08'
# api_key = Sys.getenv('ENTSOE_KEY')
# api_key = ''

dam_prices <- entsoe_dam_prices(country, from_data, to_data, verbose = TRUE, plot = TRUE)
# dam_prices %>% head()
dam_prices


# con_df = parse_ftp_links(con)
# con_df

# OK


# 2. SFTP Method ---------------------------------------------

## entsoe_list_folders ----------------------

dts_1 = entsoe_list_folders()
dts_1 %>% head()
# req = entsoe_create_url_folders(user = Sys.getenv('ENTSOE_USER'), psw = Sys.getenv('ENTSOE_PASSWORD'))
# con = rawToChar(req$content)
# con_df = parse_ftp_links(con)
# con_df

# OK



## entsoe_list_files ----------------------

dts_2 = entsoe_list_files(basis_name = 'AggregatedGenerationPerType_16.1.B_C')
dts_2 %>% head()

# req = entsoe_create_url_files(basis_name = 'DayAheadPrices_12.1.D')
# con = rawToChar(req$content)
# con_df = parse_ftp_files(con)
# con_df

# OK



## entsoe_get_file ----------------------

dts_3 = entsoe_download_file(basis_name = 'EnergyPrices_12.1.D_r3', year = 2024, month = 01, output_file = 'prova.csv')

dts = fread('prova.csv')

# basis_name = 'DayAheadPrices_12.1.D'
# year = lubridate::year(Sys.Date())
# month = lubridate::month(Sys.Date() - 31)
# output_file = 'prova.csv'
#
# file_url <- paste0("sftp://sftp-transparency.entsoe.eu/TP_export/",
#                    basis_name, "/",
#                    year, "_", sprintf("%02d", month), "_", basis_name, ".csv")
# h <- curl::new_handle()
# curl::handle_setopt(h,
#                     .list = list(
#                         httpauth = 1,
#                         userpwd = paste0(Sys.getenv("ENTSOE_USER"), ":", Sys.getenv("ENTSOE_PASSWORD")),
#                         ftp_use_epsv = FALSE))  # Set to FALSE for SFTP
# result <- curl::curl_download(file_url, output_file, handle = h)



## entsoe_actual_generation ----------------------

dts_4 = entsoe_actual_generation(2024, 10, raw = FALSE, output_folder = "data")
dts_4 %>% head()

# Calling the function with raw = TRUE, to keep the CSV file
entsoe_actual_generation(2024, 10, raw = TRUE, output_folder = "data-raw")
