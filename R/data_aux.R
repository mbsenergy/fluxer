#' Country code ISO 3166
#'
#' A dataset containing the alpha-2 and alpha-3 digits code for countries to be used.
#'
#' @name country_codes
#' @format A data frame with X rows and Y variables:
#' \describe{
#'   \item{country}{Country description}
#'   \item{code_2}{Alpha-2 digit code ISO 3166}
#'   \item{code_3}{Alpha-3 digit code ISO 3166}
#'   \item{code_num}{Numeric code ISO 3166}
#' }
#' @source \url{https://www.iban.com/country-codes}
'country_codes'


#' Mongo Aggregation Verbs
#'
#' This data table maps short names of aggregation functions to their corresponding long names.
#'
#' @format A data.table with two columns:
#' \describe{
#'   \item{v_aggregation_short}{Short names for aggregation functions.}
#'   \item{v_aggregation_long}{Corresponding long names for aggregation functions.}
#' }
#' @examples
#' mongo_aggregationverbs
#' #    v_aggregation_short v_aggregation_long
#' # 1:                 sum                 Sum
#' # 2:                mean             Average
#' # 3:                 max            Maximum
#' # 4:                 min            Minimum
#' # 5:              length               Count
#'
#' @name mongo_aggregationverbs
'mongo_aggregationverbs'
