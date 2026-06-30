#' @export
location_code_to_granularity_geo.data.table <- function(x, location_reference = NULL){

  granularity_geo <- NULL

  if(is.null(location_reference)){
    retval <- stringr::str_extract(x[["location_code"]], "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[x[["location_code"]], on = "location_code", granularity_geo])
  }
}

#' @export
location_code_to_granularity_geo.default <- function(x, location_reference = NULL){

  granularity_geo <- NULL

  if(is.null(location_reference)){
    retval <- stringr::str_extract(x, "^[a-z]+")
    retval[retval=="norge"] <- "nation"
    return(retval)
  } else {
    return(location_reference[data.table(location_code=x), on = "location_code", granularity_geo])
  }
}

#' Convert location codes to granularity_geo
#'
#' Extracts the geographic granularity label from one or more location codes.
#' When `location_reference` is `NULL`, the granularity is derived from the
#' lowercase alphabetic prefix of the location code (e.g. `"county_nor03"` ->
#' `"county"`); the special prefix `"norge"` is mapped to `"nation"`. When a
#' reference table is supplied, the granularity is looked up directly.
#'
#' @param x A character vector of location codes, or a data.table / data.frame
#'   containing a column named `location_code`.
#' @param location_reference A data.table with columns `location_code` and
#'   `granularity_geo` to use for lookup. When `NULL` (default), granularity
#'   is inferred from the location code prefix.
#' @returns A character vector the same length as `x` (or with as many elements
#'   as there are rows in `x` when `x` is a data.table), containing the
#'   corresponding `granularity_geo` values.
#' @examples
#' csdata::location_code_to_granularity_geo(c("nation_nor", "county_nor03", "municip_nor0301"))
#'
#' library(data.table)
#' dt <- data.table(location_code = c("nation_nor", "county_nor03"))
#' csdata::location_code_to_granularity_geo(dt)
#' @export
location_code_to_granularity_geo <- function(x, location_reference = NULL){
  UseMethod("location_code_to_granularity_geo")
}

#' @export
location_code_to_iso3.data.table <- function(x){
  return(rep("nor", nrow(x)))
}

#' @export
location_code_to_iso3.default <- function(x){
  return(rep("nor", length(x)))
}

#' Convert location codes to ISO 3166-1 alpha-3 country codes
#'
#' Returns the ISO 3166-1 alpha-3 country code for each location code.
#' Currently all Norwegian location codes map to `"nor"`.
#'
#' @param x A character vector of location codes, or a data.table / data.frame
#'   containing a column named `location_code`.
#' @returns A character vector the same length as `x` (or with as many elements
#'   as there are rows in `x` when `x` is a data.table), containing the
#'   corresponding ISO 3166-1 alpha-3 country code (always `"nor"`).
#' @examples
#' csdata::location_code_to_iso3(c("nation_nor", "county_nor03", "municip_nor0301"))
#'
#' library(data.table)
#' dt <- data.table(location_code = c("nation_nor", "county_nor03"))
#' csdata::location_code_to_iso3(dt)
#' @export
location_code_to_iso3 <- function(x){
  UseMethod("location_code_to_iso3")
}

