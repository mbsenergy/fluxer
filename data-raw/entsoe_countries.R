# read raw tsv, trim the character columns and remove empty columns
entsoe_countries <- fs::path("data-raw",
                             "entsoe_countries",
                             ext = "csv") |>
    data.table::fread(encoding = "UTF-8") |>
    purrr::modify_if(is.character, trimws, which = "both") |>
    purrr::discard(~is.na(.x) |> all())

data.table::setnames(entsoe_countries, c('CODE_ENTSOE', 'CODE_EIC', 'CODE_2_AREA', 'CODE_2'))
entsoe_countries = merge(fluxer::country_codes[, .(COUNTRY, CODE_2)], entsoe_countries, by = 'CODE_2', all.y = TRUE)
data.table::setcolorder(entsoe_countries, c('COUNTRY', 'CODE_2_AREA', 'CODE_2', 'CODE_ENTSOE', 'CODE_EIC'))


# save the package data in the correct format
usethis::use_data(entsoe_countries, overwrite = TRUE)
