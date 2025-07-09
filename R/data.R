#' ENTSO-E Bidding Zone Mapping
#'
#' A dataset containing ENTSO-E bidding zone metadata used to map country or zone identifiers
#' to their EIC codes, descriptions, and time zones.
#'
#' @format A `data.table` with the following columns:
#' \describe{
#'   \item{ZONE}{Character. Short code identifying the bidding zone (e.g., "DE_50HZ", "AL").}
#'   \item{EIC}{Character. ENTSO-E EIC code used in API queries.}
#'   \item{DESCRIPTION}{Character. Human-readable name for the bidding zone.}
#'   \item{TZ}{Character. Time zone (Olson format) used for localizing timestamps.}
#' }
#'
#' @usage data(full_mappings)
#'
#' @details This mapping is used to resolve country or zone names to their corresponding
#' ENTSO-E identifiers and to ensure correct time zone localization when querying data.
#'
#' @examples
#' data(full_mappings)
#' full_mappings[ZONE == "DE_50HZ"]
#'
#' @keywords dataset
#' @export
"full_mappings"
