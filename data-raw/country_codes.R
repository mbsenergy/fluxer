# read raw tsv, trim the character columns and remove empty columns
country_codes <- file.path("data-raw", "country_codes.csv") |>
    data.table::fread(encoding = "UTF-8")

data.table::setnames(country_codes, c('COUNTRY', 'CODE_2', 'CODE_3', 'CODE_NUM'))

# save the package data in the correct format
usethis::use_data(country_codes, overwrite = TRUE)
