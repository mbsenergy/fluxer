# SETUP -----------------

box::use(data.table[...])
box::use(magrittr[...])
box::use(xts[...])
# box::use(fluxer[...])
devtools::load_all()


# DT to XTS -----------------

## 1.1 Parameters ------------
n <- 1

data_type <- 'MGP_Prezzi'
username <- "PIASARACENO"
password <- "18N15C9R"
output_dir = "data"


## 1.2 Get files available at GME folder ------------
mgp_price_files = gme_mgp_get_files(
  data_type = data_type,
  output_dir = output_dir,
  username = username,
  password = password
)

last_n_files <- tail(mgp_price_files, n)
print(last_n_files)

## 1.3 Download DATASET CLEAN------------

list_mgp_prices <- lapply(last_n_files, function(file) {
  tryCatch(
    {
      # Call mgp_download_file with explicit arguments
      mgp_download_file(
        filename = file,
        data_type = data_type,
        output_dir = output_dir,
        username = username, # FTP username for authentication
        password = password, # FTP password for authentication
        raw = FALSE
      )
    },
    error = function(e) {
      # In case of an error (e.g., failed download or processing), return NULL
      message("Error processing file: ", file, " - ", e$message)
      return(NULL)
    }
  )
})

dt_mgp_prices = rbindlist(list_mgp_prices)
print(dt_mgp_prices)


## 1.3 Convert to XTS ------------

dt_to_xts <- function(
  DT,
  col_DATE = 'DATE',
  col_HOUR = NULL,
  col_CAST,
  col_VALUE = 'VALUE',
  col_METADATA = NULL,
  verbose = FALSE
) {
  # Ensure data.table format
  setDT(DT)

  cols_to_keep = unique(na.omit(c(col_DATE, col_HOUR, col_CAST, col_VALUE)))
  DTS = DT[, ..cols_to_keep]

  if (col_DATE == "DATETIME") {
    # Use DATETIME directly if already in the correct format
    DTS[, DATETIME := as.POSIXct(get(col_DATE))]
  } else if (!is.null(col_HOUR)) {
    # Ensure HOUR is in HH:MM:SS format
    DTS[, (col_HOUR) := sprintf("%02d:00:00", as.integer(get(col_HOUR)))]

    # Merge DATE and HOUR into DATETIME
    DTS[,
      DATETIME := as.POSIXct(
        paste(get(col_DATE), get(col_HOUR)),
        format = "%Y-%m-%d %H:%M:%S"
      )
    ]
    DTS[, (col_DATE) := NULL]
    DTS[, (col_HOUR) := NULL]
  } else {
    # Use only DATE
    DTS[, DATETIME := as.Date(get(col_DATE))]
  }

  DT_wide <- dcast(DTS, DATETIME ~ get(col_CAST), value.var = col_VALUE)

  # Convert to xts
  xts_obj <- xts(DT_wide[, -1, with = FALSE], order.by = DT_wide$DATETIME)

  if (!is.null(col_METADATA)) {
    metadata_values <- lapply(col_METADATA, function(col) {
      unique(DT[[col]])
    })
    names(metadata_values) <- col_METADATA
    attr(xts_obj, "metadata") <- metadata_values
  }

  # Convert xts to "dataWithMetadata" class
  xts_obj = as_data_with_metadata(xts_obj, metadata = attr(xts_obj, "metadata"))

  return(xts_obj)
}

xt_mgp_prices = dt_to_xts(
  DT = dt_mgp_prices,
  col_DATE = 'DATE',
  col_HOUR = 'HOUR',
  col_CAST = 'ZONE',
  col_VALUE = 'VALUE',
  verbose = TRUE
)
