box::use(data.table[...])
box::use(magrittr[...])
box::use(ggplot2[...])
box::use(reticulate[...])
devtools::load_all()
# box::use(fluxer[...])

# 1. API Method ---------------------------------------------

## DAM Prices ----------------------
install_flux_entsoe()

country <- "IT_CALA"
from_data <- '20240101'
to_data <- '20240201'
# api_key = Sys.getenv('ENTSOE_KEY')
# api_key = ''

dam_prices <- entsoe_dam_prices(country, from_data, to_data)
dam_prices


## Actual Generation ----------------------
country <- "IT_CNOR"
from_data <- '20240101'
to_data <- '20240201'
# api_key = Sys.getenv('ENTSOE_KEY')
# api_key = ''

act_gen <- entsoe_actual_generation(country, from_data, to_data)
act_gen
