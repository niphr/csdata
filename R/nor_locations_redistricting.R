#' Redistricting weights for Norwegian geographic units
#'
#' Returns a data.table of weighting factors used to convert historical data
#' recorded under old administrative boundaries to the 2024 borders.
#' Each row maps an original location code (as of a given calendar year) to
#' the current location code, with a proportional weighting.
#'
#' @param border Integer. The target geographic border year. Valid values:
#'   `2024`. Defaults to `csdata::config$border_nor`.
#' @returns A data.table with columns:
#'   \describe{
#'     \item{location_code_current}{Location code under the target border year.
#'       Each of the 421 distinct values also appears in
#'       [nor_locations_names()].}
#'     \item{location_code_original}{Location code as it existed in `calyear`.
#'       Of the 941 distinct values, 520 are retired codes that
#'       [nor_locations_names()] no longer lists.}
#'     \item{calyear}{The calendar year to which `location_code_original`
#'       applies.}
#'     \item{weighting}{Proportional weight to apply when aggregating from the
#'       original location to the current location. Values sum to 1 within each
#'       `location_code_original` / `calyear` group (all 40,819 of them), so a
#'       row spread across several current locations is split rather than
#'       duplicated. They need not sum to 1 within a `location_code_current` /
#'       `calyear` group: 4,334 of those 35,160 groups do not.}
#'     \item{granularity_geo}{Geographic granularity: one of `"nation"`,
#'       `"county"`, `"municip"`, `"wardbergen"`, `"wardoslo"`,
#'       `"wardstavanger"`, `"wardtrondheim"`, `"extrawardoslo"`,
#'       `"missingwardbergen"`, `"missingwardoslo"`, `"missingwardstavanger"`,
#'       `"missingwardtrondheim"`, `"notmainlandcounty"`,
#'       `"notmainlandmunicip"`, `"missingcounty"`, `"missingmunicip"`.}
#'   }
#' @source Statistics Norway (SSB) municipal reform documentation.
#' @seealso No vignette covers this function.
#'   \code{vignette("locations_norway", package = "csdata")} tabulates the
#'   `location_code` values returned by [nor_locations_names()], which are the
#'   values `location_code_current` is drawn from.
#' @examples
#' d <- csdata::nor_locations_redistricting()
#' head(d)
#' d[calyear == 2019 & granularity_geo == "municip"]
#' @export
nor_locations_redistricting <- function(
  border = csdata::config$border_nor
) {
  stopifnot(border == 2024)
  x <- get0("nor_locations_redistricting_b2024", envir = asNamespace("csdata"))
  d <- copy(x)
  return(d)
}
