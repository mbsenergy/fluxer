# read raw tsv, trim the character columns and remove empty columns
asset_types <- file.path("data-raw", "asset_types.rds") |>
    readRDS()


maptype <- list(
    "Fossil Oil" = "Thermal",
    "Biomass" = "Thermal",
    "Hydro Run-of-river and poundage" = "Hydro (River)",
    "Fossil Gas" = "Thermal",
    "Other" = "Thermal",
    "Solar" = "Photovoltaic",
    "Wind Onshore" = "Wind",
    "Fossil Hard coal" = "Thermal",
    "Hydro Water Reservoir" = "Hydro",
    "Fossil Coal-derived gas" = "Thermal",
    "Hydro Pumped Storage" = "Pumps Consumption",
    "Waste" = "Thermal",
    "Geothermal" = "Geothermal"
)

# Convert the list to a data.frame
maptype_df <- data.frame(
    DEFINITION = names(maptype),
    ASSET_CATEGORY = unlist(maptype),
    stringsAsFactors = FALSE
)

asset_types = merge(asset_types, maptype_df, by = 'DEFINITION')
setDT(asset_types)
setcolorder(asset_types, c('CODE', 'DEFINITION', 'ASSET_CATEGORY', 'DESCRIPTION'))
setnames(asset_types, 'DEFINITION', 'PRODUCTION_TYPE')
# save the package data in the correct format
usethis::use_data(asset_types, overwrite = TRUE)
