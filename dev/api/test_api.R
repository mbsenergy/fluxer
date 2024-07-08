
# API ENV --------------------

# CALL: Filter, Aggregate and Merge Collection --------------------

devtools::load_all()

dt_gdp = query_mongo(collection_name = 'EC01_GDP')
dt_cpi = query_mongo(collection_name = 'EC01_CPI')
