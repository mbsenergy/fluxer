#' @title
#' List of Mapped Countries for DAM Prices
#'
#' @description
#' This data table contains a mapping of countries to their respective EIC codes for retrieving DAM prices from the ENTSOE Transparency Platform API.
#'
#' @examples
#' library(entsoeapi)
#' entsoe_countries
#'
'entsoe_countries'


#' Download ENTSO-E Day-Ahead Market Prices
#'
#' Retrieves day-ahead electricity market prices for a given ENTSO-E bidding zone and date range
#' using the `entsoe-py` Python package through `reticulate`.
#'
#' @param country Character. ENTSO-E EIC code for the bidding zone (e.g., `"10YDE-VE-------2"` for 50Hertz).
#' @param from_data Character or Date. Start date of the query in `"YYYY-MM-DD"` format.
#' @param to_data Character or Date. End date of the query in `"YYYY-MM-DD"` format.
#'
#' @return A `data.table` with the columns:
#' \describe{
#'   \item{ENTSOE_CODE}{The country EIC code used in the query.}
#'   \item{DATE}{Date of the price (class `IDate`).}
#'   \item{TIME}{Hour of the day as a string (`"HH:MM:SS"`).}
#'   \item{HOUR}{Hour of the day as an integer (0–23).}
#'   \item{VALUE}{Price in EUR/MWh.}
#'   \item{UNIT}{Price unit (always `"€/MWh"`).}
#' }
#'
#' If the download fails or returns no data, returns `NULL` and displays a colored warning message.
#'
#' @details Requires an environment variable `ENTSOE_KEY` to be set with a valid ENTSO-E API key.
#' Automatically creates and manages a virtual Python environment to run `entsoe-py`.
#'
#' @import data.table
#' @importFrom crayon green red yellow blue bgGreen bgRed
#' @importFrom reticulate py_run_string py virtualenv_exists virtualenv_create virtualenv_install use_virtualenv
#' @importFrom lubridate hour
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ENTSOE_KEY = "your_api_key")
#' prices = entsoe_dam_prices("10YDE-VE-------2", "2024-01-01", "2024-01-05")
#' print(prices)
#' }
#'
#' @export
entsoe_dam_prices = function(country, from_data, to_data) {
  # Format timestamps
  start = from_data
  end = to_data

  # API key
  api_key = Sys.getenv("ENTSOE_KEY")
  if (api_key == "") {
    stop("Set ENTSOE_KEY environment variable.")
  }

  # Python code with try-except
  reticulate::py_run_string(sprintf(
    "
from entsoe import EntsoePandasClient
import pandas as pd

try:
    start = pd.Timestamp('%s', tz='Europe/Brussels')
    end = pd.Timestamp('%s', tz='Europe/Brussels')
    client = EntsoePandasClient(api_key='%s')
    df_series = client.query_day_ahead_prices('%s', start=start, end=end)
    df = df_series.reset_index()
    df.columns = ['DATETIME', 'VALUE']
    df = df[['DATETIME', 'VALUE']]
    error_msg = ''
except Exception as e:
    df = None
    error_msg = str(e)
",
    start,
    end,
    api_key,
    country
  ))

  # Check for errors
  if (!is.null(py$error_msg) && py$error_msg != "") {
    message(crayon::red("[ERROR] ENTSO-E download failed: "), py$error_msg)
    return(NULL)
  }

  dts = py$df

  if (is.null(dts) || nrow(dts) == 0) {
    message(
      crayon::bgRed("[WARNING]"),
      crayon::blue("ENTSO-E DAM prices NOT available for "),
      crayon::yellow(country),
      crayon::blue(" from "),
      crayon::green(from_data),
      crayon::blue(" to "),
      crayon::green(to_data),
      crayon::yellow(sprintf(" (%s rows).", nrow(dts)))
    )
    return(NULL)
  }

  setDT(dts)

  dts[,
    c("DATE", "TIME", "HOUR") := .(
      as.IDate(DATETIME),
      format(DATETIME, "%H:%M:%S"),
      hour(DATETIME)
    )
  ]
  dts[, DATETIME := NULL]
  dts[, ENTSOE_CODE := country]
  dts[, UNIT := '€/MWh']

  setcolorder(dts, c('ENTSOE_CODE', 'DATE', 'TIME', 'HOUR', 'VALUE', 'UNIT'))
  setorderv(dts, c('ENTSOE_CODE', 'DATE', 'TIME', 'HOUR'))

  n_row = nrow(dts)

  message(
    crayon::bgGreen("[OK]"),
    crayon::blue("ENTSO-E DAM prices downloaded successfully for "),
    crayon::yellow(country),
    crayon::blue(" from "),
    crayon::green(from_data),
    crayon::blue(" to "),
    crayon::green(to_data),
    crayon::yellow(sprintf(" (%s rows).", nrow(dts)))
  )

  return(dts)
}


#' Download Actual Generation Data from ENTSO-E Transparency Platform
#'
#' Downloads hourly net generation data by production type from the ENTSO-E Transparency API
#' for a specified country and time range. Requires a valid API key set in the `ENTSOE_KEY`
#' environment variable.
#'
#' @param country Character. ENTSO-E bidding zone code (e.g., `"IT_CALA"` for South Italy).
#' @param from_data Character or Date. Start date in `"YYYY-MM-DD"` or `"YYYYMMDD"` format.
#' @param to_data Character or Date. End date in `"YYYY-MM-DD"` or `"YYYYMMDD"` format.
#'
#' @return A `data.table` with columns:
#' \describe{
#'   \item{ENTSOE_CODE}{Bidding zone code.}
#'   \item{PRODUCTION_TYPE}{Generation technology name.}
#'   \item{DATE}{Date (as `IDate`).}
#'   \item{TIME}{Time (as `"HH:MM:SS"` string).}
#'   \item{HOUR}{Integer hour (0–23).}
#'   \item{VALUE}{Net generation in megawatts (MW).}
#'   \item{UNIT}{Always `"Net Generation [MW]"`.}
#' }
#'
#' @note Requires the Python packages `entsoe-py` and `pandas`, and a valid ENTSO-E API key
#' set in the environment variable `ENTSOE_KEY`.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(ENTSOE_KEY = "your_api_key")
#' data = entsoe_actual_generation("DE_LU", "2023-01-01", "2023-01-02")
#' }
#'
#' @export
entsoe_actual_generation = function(country, from_data, to_data) {
  # Format timestamps
  start = from_data
  end = to_data

  # API key
  api_key = Sys.getenv("ENTSOE_KEY")
  if (api_key == "") {
    stop("Set ENTSOE_KEY environment variable.")
  }

  # Python code with try-except
  reticulate::py_run_string(sprintf(
    "
from entsoe import EntsoePandasClient
import pandas as pd

try:
    start = pd.Timestamp('%s', tz='Europe/Brussels')
    end = pd.Timestamp('%s', tz='Europe/Brussels')
    client = EntsoePandasClient(api_key='%s')
    df_series = client.query_generation('%s', start=start, end=end, psr_type=None)
    df = df_series.reset_index()
    error_msg = ''
except Exception as e:
    df = None
    error_msg = str(e)
",
    start,
    end,
    api_key,
    country
  ))

  # Check for errors
  if (!is.null(py$error_msg) && py$error_msg != "") {
    message(crayon::red("[ERROR] ENTSO-E download failed: "), py$error_msg)
    return(NULL)
  }

  dts = py$df

  if (is.null(dts) || nrow(dts) == 0) {
    message(
      crayon::bgRed("[WARNING]"),
      crayon::blue("ENTSO-E Actual Generation NOT available for "),
      crayon::yellow(country),
      crayon::blue(" from "),
      crayon::green(from_data),
      crayon::blue(" to "),
      crayon::green(to_data),
      crayon::yellow(sprintf(" (%s rows).", nrow(dts)))
    )
    return(NULL)
  }

  setDT(dts)
  dts = melt(
    dts,
    id.vars = 'index',
    variable.name = 'PRODUCTION_TYPE',
    value.name = 'VALUE'
  )

  dts[,
    c("DATE", "TIME", "HOUR") := .(
      as.IDate(index),
      format(index, "%H:%M:%S"),
      hour(index)
    )
  ]

  dts[, index := NULL]
  dts[, ENTSOE_CODE := country]
  dts[, UNIT := 'Net Generation [MW]']

  setcolorder(
    dts,
    c('ENTSOE_CODE', 'PRODUCTION_TYPE', 'DATE', 'TIME', 'HOUR', 'VALUE', 'UNIT')
  )
  setorderv(dts, c('ENTSOE_CODE', 'PRODUCTION_TYPE', 'DATE', 'TIME', 'HOUR'))

  n_row = nrow(dts)

  message(
    crayon::bgGreen("[OK]"),
    crayon::blue("ENTSO-E Actual Generation downloaded successfully for "),
    crayon::yellow(country),
    crayon::blue(" from "),
    crayon::green(from_data),
    crayon::blue(" to "),
    crayon::green(to_data),
    crayon::yellow(sprintf(" (%s rows).", nrow(dts)))
  )

  return(dts)
}
