#' @export
add_granularity_geo_to_data_set.data.table <- function(
  x,
  location_reference = NULL
) {
  granularity_geo <- NULL

  x[,
    granularity_geo := location_code_to_granularity_geo(
      x,
      location_reference = location_reference
    )
  ]
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
#' @param x A data.table containing a column named `location_code`. Only a
#'   data.table method exists, so any other class raises "no applicable
#'   method".
#' @param location_reference A data.table with columns `location_code` and
#'   `granularity_geo` to use for lookup. When `NULL` (default), granularity
#'   is derived from the location code prefix (e.g. `"county_nor03"` -> `"county"`).
#'   Its `location_code` values should be unique. A code that appears twice in
#'   the reference produces more labels than `x` has rows, and the assignment
#'   then fails with "Supplied N items to be assigned to M items of column
#'   'granularity_geo'". [nor_locations_names()] holds one such code today,
#'   `"lab_nor084467"`, which two laboratories share.
#' @returns `x`, invisibly, with the `granularity_geo` column added or updated.
#' @family data set column adders
#' @seealso No vignette covers this function. [location_code_to_granularity_geo()]
#'   returns the same labels as a vector instead of writing them onto `x`.
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
add_granularity_geo_to_data_set <- function(x, location_reference = NULL) {
  UseMethod("add_granularity_geo_to_data_set")
}

#' @export
add_iso3_to_data_set.data.table <- function(x) {
  iso3 <- NULL

  x[, iso3 := location_code_to_iso3(x)]
  data.table::shouldPrint(x)
  invisible(x)
}

#' Add an iso3 column to a data set
#'
#' Adds an `iso3` column holding the ISO 3166-1 alpha-3 country code,
#' modifying `x` in place. csdata carries Norwegian data only, so the value is
#' `"nor"` on every row. The column contents are not read, so `x` gains the
#' column even when it has no `location_code`.
#'
#' @param x A data.table. Only a data.table method exists, so any other class
#'   raises "no applicable method".
#' @returns `x`, invisibly, with a new `iso3` column holding `"nor"` on every
#'   row.
#' @family data set column adders
#' @seealso No vignette covers this function. [location_code_to_iso3()] returns
#'   the same values as a vector instead of writing them onto `x`.
#' @examples
#' library(data.table)
#' data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
#' csdata::add_iso3_to_data_set(data)
#' print(data)
#' @export
add_iso3_to_data_set <- function(x) {
  UseMethod("add_iso3_to_data_set")
}
