
# SETUP -----------------

box::use(data.table[...])
box::use(magrittr[...])
box::use(DBI[...])
box::use(xml2[...])
# Load the package containing alba_download_data
devtools::load_all()



# GET MIN AND MAX DATE  -----------------

db_get_minmax_dates

ret_files = c(
    "2024-12-25", "2024-12-28", "2024-12-31"
)



# Complete Dates from Vector  -----------------

# Find all missing dates (full week)
missing_dates_full = find_missing_dates(ret_files, full_week = TRUE)

# Find only missing weekdays (Monday to Friday)
missing_dates_weekdays = find_missing_dates(ret_files, full_week = FALSE)

# View results
print("All missing dates:")
print(missing_dates_full)

print("Missing weekdays only:")
print(missing_dates_weekdays)
