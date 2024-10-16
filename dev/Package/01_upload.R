

# Packages ----------------------------------
box::use(data.table[...])
box::use(devtools[...])
box::use(duckdb[...])


# Tables

dt_zone <- fread("dt_zone.csv", header = T)

