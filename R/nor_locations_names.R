
#' Location codes and names for Norwegian geographic units
#'
#' Returns a data.table of all Norwegian geographic units with their location
#' codes, display names, and presentation metadata. Coverage includes nation,
#' counties, municipalities, city districts (Oslo, Bergen, Stavanger,
#' Trondheim), BA-regions, and lab regions.
#'
#' @param border Integer. The geographic border year determining which
#'   administrative boundaries are used. Valid values: `2024`.
#'   Defaults to `csdata::config$border_nor`.
#' @returns A data.table with one row per geographic unit and the columns:
#'   \describe{
#'     \item{location_code}{Location code (e.g. `"nation_nor"`, `"county_nor03"`).}
#'     \item{location_name}{Full location name.}
#'     \item{location_name_short}{Abbreviated name: 1-letter for nation and
#'       county, shorter display name for Oslo and Bergen city districts.}
#'     \item{location_name_description_nb}{Location name with a parenthetical
#'       description of geographic level (Norwegian Bokmal).}
#'     \item{location_name_file_nb_utf}{Name suitable for use in file names,
#'       retaining Norwegian characters.}
#'     \item{location_name_file_nb_ascii}{Name suitable for use in file names,
#'       with Norwegian characters replaced by ASCII equivalents.}
#'     \item{location_order}{Integer giving the preferred presentation order.}
#'     \item{granularity_geo}{Geographic granularity: one of `"nation"`,
#'       `"georegion"`, `"mtregion"`, `"county"`, `"municip"`, `"baregion"`,
#'       `"wardoslo"`, `"wardbergen"`, `"wardstavanger"`, `"wardtrondheim"`,
#'       `"extrawardoslo"`, `"lab"`.}
#'   }
#' @source \url{https://no.wikipedia.org/wiki/Liste_over_norske_kommunenummer}
#' @examples
#' d <- nor_locations_names()
#' head(d)
#' d[granularity_geo == "county"]
#' @export
nor_locations_names <- function(border = csdata::config$border_nor){
  stopifnot(border == 2024)
  x <- get0("nor_locations_names_b2024", envir = asNamespace("csdata"))
  d <- copy(x)
  return(d)
}


