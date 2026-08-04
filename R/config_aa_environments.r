#' Norwegian characters in unicode
#'
#' A named list of Norwegian special characters as unicode strings,
#' for use where literal non-ASCII characters are inconvenient.
#' Elements: `AA`/`aa` (Aa/aa), `OE`/`oe` (Oe/oe), `AE`/`ae` (Ae/ae).
#' Each element is a single character.
#' @family unicode character lists
#' @seealso No vignette covers this object.
#' @examples
#' print(csdata::nb)
#' csdata::nb$AA  # uppercase Aa
#' @export nb
nb <- list()
nb$AA <- "\u00C5"
nb$aa <- "\u00E5"
nb$OE <- "\u00D8"
nb$oe <- "\u00F8"
nb$AE <- "\u00C6"
nb$ae <- "\u00E6"

#' Swedish characters in unicode
#'
#' A named list of Swedish special characters as unicode strings,
#' for use where literal non-ASCII characters are inconvenient.
#' Elements: `OE`/`oe` (Oe/oe), `AE`/`ae` (Ae/ae). Each element is a single
#' character. These four names are spelled the same way in [nb], so code that
#' indexes `se` by name also works against `nb`.
#' @family unicode character lists
#' @seealso No vignette covers this object.
#' @examples
#' print(csdata::se)
#' csdata::se$oe  # lowercase oe
#' @export se
se <- list()
se$OE <- "\u00D6"
se$oe <- "\u00F6"
se$AE <- "\u00C4"
se$ae <- "\u00E4"

#' Package configuration environment
#'
#' An environment that stores package-level configuration variables.
#' Modify via [set_config()]; read directly (e.g. `config$border_nor`).
#'
#' Available variables:
#' - `border_nor` (default `2024`): the border year used when selecting
#'   Norwegian geographic datasets. Valid values: `2024`.
#'
#' `border_nor` supplies the default `border` argument of
#' [nor_locations_names()], [nor_locations_redistricting()],
#' [nor_locations_hierarchy_from_to()], [nor_population_by_age_cats()] and
#' [nor_population_by_sex_age_cats()]. No other exported function reads it.
#' @seealso No vignette covers this object.
#' @examples
#' print(ls(csdata::config))
#' for(i in names(csdata::config)){
#'   cat(i, ":", csdata::config[[i]], "\n")
#' }
#' @export
config <- new.env()
config$border_nor <- 2024

#' Set package configuration options
#'
#' Updates `border_nor` in the [config] environment. Call this at the start of
#' a script to change the default `border` argument of
#' [nor_locations_names()], [nor_locations_redistricting()],
#' [nor_locations_hierarchy_from_to()], [nor_population_by_age_cats()] and
#' [nor_population_by_sex_age_cats()].
#'
#' @param border_nor Integer. The Norwegian geographic border year to use as
#'   the default. Valid values: `2024`. Pass `NULL` to leave unchanged.
#' @returns Invisibly, the value assigned to `config$border_nor`, or `NULL`
#'   when `border_nor` is `NULL`. Called for the side effect of updating
#'   [config].
#' @seealso No vignette covers this function.
#' @examples
#' old <- csdata::config$border_nor
#' csdata::set_config(border_nor = 2024)
#' csdata::config$border_nor  # 2024
#' csdata::set_config(border_nor = old)  # restore
#' @export
set_config <- function(border_nor = NULL) {
  if (!is.null(border_nor)) {
    stopifnot(border_nor == 2024)
    config$border_nor <- border_nor
  }
}
