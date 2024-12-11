# read raw tsv, trim the character columns and remove empty columns
italy_zones <- fs::path("data-raw",
                             "italy_zones",
                             ext = "csv") |>
    data.table::fread(encoding = "UTF-8") |>
    purrr::modify_if(is.character, trimws, which = "both") |>
    purrr::discard(~is.na(.x) |> all())

data.table::setnames(italy_zones, c('AREA', 'AREA_TERNA'))

# save the package data in the correct format
usethis::use_data(italy_zones, overwrite = TRUE)
