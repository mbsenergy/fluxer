# read raw tsv, trim the character columns and remove empty columns
entsoe_countries <- fs::path("data-raw",
                             "entsoe_countries",
                             ext = "csv") |>
    data.table::fread(encoding = "UTF-8") |>
    purrr::modify_if(is.character, trimws, which = "both") |>
    purrr::discard(~is.na(.x) |> all())

data.table::setnames(entsoe_countries, c('COUNTRY', 'EIC_CODE'))

# save the package data in the correct format
usethis::use_data(entsoe_countries, overwrite = TRUE)
