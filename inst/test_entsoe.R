box::use(data.table[...])
box::use(magrittr[...])
box::use(ggplot2[...])
box::use(reticulate[...])
devtools::load_all()
# box::use(fluxer[...])

# 1. API Method ---------------------------------------------

## DAM Prices ----------------------
env = reticulate::virtualenv_create("entenv")
reticulate::virtualenv_create(env)
reticulate::virtualenv_install(env, packages = c("pandas", "entsoe-py"))
reticulate::use_virtualenv(env, required = TRUE)

country <- "DE_50HZ"
from_data <- '20240101'
to_data <- '20240201'

dam_prices <- entsoe_dam_prices(country, from_data, to_data)
dam_prices


## Actual Generation ----------------------
country = fluxer::full_mappings$ZONE[10]
from_data <- '20240101'
to_data <- '20240201'

act_gen <- entsoe_actual_generation(country, from_data, to_data)
act_gen