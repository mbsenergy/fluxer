
# Packages -------------------------------------
box::use(data.table[...],
         mongolite[...],
         magrittr[...])


# Connect to MONGO -------------------------------
URL <- Sys.getenv('MONGO_URI')

mongo_connection = mongo(url = paste0(URL, 'admin'))


## DB -------------------------
databases = mongo_connection$run('{"listDatabases": 1}')
database_info = as.data.table(databases$databases)

database_name = databases$databases$name
database_name = database_name[!database_name %in% c('admin', 'local', 'BACKEND')]

## Collections -------------------------
list_collections = function(mongo_url, database_name) {
    db_connection = mongo(url = paste0(mongo_url, database_name))
    collections = db_connection$run('{"listCollections": 1}')
    collection_names = collections$cursor$firstBatch$name
    return(collection_names)
}

collections_list = lapply(database_name, function(db_name) {
    collections = list_collections(URL, db_name)
    data.table::data.table(DB = db_name, COLLECTION = collections)
})

names(collections_list) = database_name


## Create DT ------------------------------------------------------
collections_dt = data.table::rbindlist(collections_list)
collections_dt = collections_dt %>%
    merge(database_info, by.x = 'DB', by.y = 'name', all.x = TRUE)


## Insert to MONGODB ------------------------------------------------

# Connect to a MongoDB collection where you want to insert this data
# Replace 'your_collection_name' with the actual collection name
output_collection = mongo(collection = "flux_info", url = paste0(URL, 'BACKEND'))

# Insert the data into MongoDB
output_collection$insert(collections_dt)

print("Data has been loaded into MongoDB successfully.")
