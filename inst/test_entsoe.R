
box::use(data.table[...])
box::use(magrittr[...])
devtools::load_all()


# entsoe_list_folders ----------------------

dts_1 = entsoe_list_folders()
dts_1 %>% head()
# req = entsoe_create_url_folders(user = Sys.getenv('ENTSOE_USER'), psw = Sys.getenv('ENTSOE_PASSWORD'))
# con = rawToChar(req$content)
# con_df = parse_ftp_links(con)
# con_df

# OK



# entsoe_list_files ----------------------

dts_2 = entsoe_list_files(basis_name = 'DayAheadPrices_12.1.D')
dts_2 %>% head()

# req = entsoe_create_url_files(basis_name = 'DayAheadPrices_12.1.D')
# con = rawToChar(req$content)
# con_df = parse_ftp_files(con)
# con_df

# OK



# entsoe_get_file ----------------------

dts_3 = download_entsoe_file(basis_name = 'DayAheadPrices_12.1.D', year = 2024, month = 10, output_file = 'prova.csv')

basis_name = 'DayAheadPrices_12.1.D'
year = lubridate::year(Sys.Date())
month = lubridate::month(Sys.Date() - 31)
output_file = 'prova.csv'

file_url <- paste0("sftp://sftp-transparency.entsoe.eu/TP_export/",
                   basis_name, "/",
                   year, "_", sprintf("%02d", month), "_", basis_name, ".csv")
h <- curl::new_handle()
curl::handle_setopt(h,
                    .list = list(
                        httpauth = 1,
                        userpwd = paste0(Sys.getenv("ENTSOE_USER"), ":", Sys.getenv("ENTSOE_PASSWORD")),
                        ftp_use_epsv = FALSE))  # Set to FALSE for SFTP
result <- curl::curl_download(file_url, output_file, handle = h)
