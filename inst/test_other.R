
# SETUP -----------------

box::use(data.table[...])
box::use(magrittr[...])
box::use(curl[...])
box::use(xml2[...])
# Load the package containing alba_download_data
devtools::load_all()



# Alba  -----------------

## 1.1 Parameters ------------
output_dir <- file.path('inst', "data")
username <- "checchi"
password <- "2bczpsNH"
from_date <- Sys.Date() - 5
to_date <- Sys.Date()
type = 'rest'

## 1.2 Download DATASET power ------------

result_power <- alba_download_data_power(
  from_date = from_date,
  to_date = to_date,
  type = type,
  output_dir = output_dir,
  username = username,
  password = password
)

print(result_power)


## 1.3 Download DATASET gas ------------

result_gas <- alba_download_data_gas(
  from_date = from_date,
  to_date = to_date,
  type = type,
  output_dir = output_dir,
  username = username,
  password = password
)

print(result_gas)

