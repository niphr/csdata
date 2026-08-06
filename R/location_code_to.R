#' @export
location_code_to_granularity_geo.data.table <- function(
  x,
  location_reference = NULL
) {
  granularity_geo <- NULL

  if (is.null(location_reference)) {
    retval <- stringr::str_extract(x[["location_code"]], "^[a-z]+")
    retval[retval == "norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[
      x[["location_code"]],
      on = "location_code",
      granularity_geo
    ])
  }
}

#' @export
location_code_to_granularity_geo.default <- function(
  x,
  location_reference = NULL
) {
  granularity_geo <- NULL

  if (is.null(location_reference)) {
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval == "norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[
      data.table(location_code = x),
      on = "location_code",
      granularity_geo
    ])
  }
}

#' Convert location codes to granularity_geo
#'
#' Extracts the geographic granularity label from one or more location codes.
#' When `location_reference` is `NULL`, the granularity comes from the
#' lowercase alphabetic prefix of the location code (e.g. `"county_nor03"` ->
#' `"county"`). The prefix `"norge"` is the one special case, and it gives
#' `"nation"`. When you supply a reference table, the granularity comes from
#' that table.
#'
#' @param x A character vector of location codes, or a data.table with a
#'   column named `location_code`. A plain data.frame is not supported: it
#'   falls through to the default method, which treats the frame itself as the
#'   vector of codes.
#' @param location_reference A data.table with columns `location_code` and
#'   `granularity_geo` to use for lookup. When `NULL` (default), granularity
#'   comes from the location code prefix.
#' @returns A character vector of the corresponding `granularity_geo` values.
#'   It has the same length as `x`, or as many elements as `x` has rows when
#'   `x` is a data.table. A code with no leading lowercase letters yields `NA`.
#'   An unmatched code also yields `NA` when you supply `location_reference`.
#'
#'   The length guarantee has one exception. `location_reference` is joined on
#'   `location_code`, so a code that appears more than once in the reference
#'   contributes one element per matching row. [nor_locations_names()] holds
#'   one such code today, `"lab_nor084467"`, which two laboratories share.
#' @family location code converters
#' @seealso \code{vignette("csdata", package = "csdata")}, which calls this
#'   function on a worked example.
#'   \code{vignette("locations_norway", package = "csdata")} tabulates the
#'   `location_code` values returned by [nor_locations_names()].
#' @examples
#' csdata::location_code_to_granularity_geo(c("nation_nor", "county_nor03", "municip_nor0301"))
#'
#' # a code with no lowercase prefix gives NA
#' csdata::location_code_to_granularity_geo(c("nation_nor", "", NA))
#'
#' library(data.table)
#' dt <- data.table(location_code = c("nation_nor", "county_nor03"))
#' csdata::location_code_to_granularity_geo(dt)
#'
#' # looked up against a reference table instead of parsed from the prefix
#' csdata::location_code_to_granularity_geo(
#'   c("nation_nor", "county_nor03", "blah"),
#'   location_reference = csdata::nor_locations_names()
#' )
#' @export
location_code_to_granularity_geo <- function(x, location_reference = NULL) {
  UseMethod("location_code_to_granularity_geo")
}

#' @export
location_code_to_iso3.data.table <- function(x) {
  return(rep("nor", nrow(x)))
}

#' @export
location_code_to_iso3.default <- function(x) {
  return(rep("nor", length(x)))
}

#' Convert location codes to ISO 3166-1 alpha-3 country codes
#'
#' Returns the ISO 3166-1 alpha-3 country code for each location code. csdata
#' carries Norwegian data only, so the implementation returns `"nor"` for every
#' element without inspecting its value. A code from another country, or a
#' string that is not a location code at all, also returns `"nor"`.
#'
#' @param x A character vector of location codes, or a data.table with a
#'   column named `location_code`. A plain data.frame is not supported: it
#'   falls through to the default method, which returns one value per column
#'   rather than one per row.
#' @returns A character vector of `"nor"`. It has the same length as `x`, or
#'   as many elements as `x` has rows when `x` is a data.table.
#' @family location code converters
#' @seealso No vignette covers this function.
#'   \code{vignette("locations_norway", package = "csdata")} tabulates the
#'   `location_code` values returned by [nor_locations_names()].
#' @examples
#' csdata::location_code_to_iso3(c("nation_nor", "county_nor03", "municip_nor0301"))
#'
#' # the input value is not inspected
#' csdata::location_code_to_iso3(c("county_nor03", "xyz"))
#'
#' library(data.table)
#' dt <- data.table(location_code = c("nation_nor", "county_nor03"))
#' csdata::location_code_to_iso3(dt)
#' @export
location_code_to_iso3 <- function(x) {
  UseMethod("location_code_to_iso3")
}
