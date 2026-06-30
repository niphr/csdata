#' @export
add_granularity_geo_to_data_set.data.table <- function(x, location_reference = NULL){
  granularity_geo <- NULL

  x[, granularity_geo := location_code_to_granularity_geo(x, location_reference = location_reference)]
  data.table::shouldPrint(x)
  invisible(x)
}

#' Add a granularity_geo column to a data set
#'
#' Derives the geographic granularity label from the `location_code` column
#' and adds it as a new `granularity_geo` column, modifying `x` in place.
#' When `location_reference` is `NULL` the granularity is inferred from the
#' location code prefix; when a reference table is supplied, it is looked up
#' directly.
#'
#' @param x A data.table containing a column named `location_code`.
#' @param location_reference A data.table with columns `location_code` and
#'   `granularity_geo` to use for lookup. When `NULL` (default), granularity
#'   is derived from the location code prefix (e.g. `"county_nor03"` -> `"county"`).
#' @returns `x`, invisibly, with the `granularity_geo` column added or updated.
#' @examples
#' library(data.table)
#' data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
#' csdata::add_granularity_geo_to_data_set(data)
#' print(data)
#'
#' library(data.table)
#' data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
#' csdata::add_granularity_geo_to_data_set(data, location_reference = csdata::nor_locations_names())
#' print(data)
#' @export
add_granularity_geo_to_data_set <- function(x, location_reference = NULL){
  UseMethod("add_granularity_geo_to_data_set")
}

#' @export
add_iso3_to_data_set.data.table <- function(x){
  granularity_geo <- NULL

  x[, granularity_geo := location_code_to_iso3(x)]
  data.table::shouldPrint(x)
  invisible(x)
}

#' Add an iso3 column to a data set
#'
#' Derives the ISO 3166-1 alpha-3 country code from the `location_code` column
#' and adds it as a new column, modifying `x` in place. Currently all Norwegian
#' location codes map to `"nor"`.
#'
#' @param x A data.table containing a column named `location_code`.
#' @returns `x`, invisibly, with a new `granularity_geo` column containing the
#'   ISO 3166-1 alpha-3 country code (always `"nor"` for Norwegian locations).
#' @examples
#' library(data.table)
#' data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
#' csdata::add_iso3_to_data_set(data)
#' print(data)
#' @export
add_iso3_to_data_set <- function(x){
  UseMethod("add_iso3_to_data_set")
}
