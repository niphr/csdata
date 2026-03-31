#' All redistricting in Norway
#'
#' This function returns a dataset that is used to transfer "original" datasets
#' to the 2020 or 2024 borders.
#'
#' @param border The year in which Norwegian geographical boundaries were designated (2020, 2024).
#' @returns
#' \describe{
#' \item{location_code_current}{The location code per today.}
#' \item{location_code_original}{The location code as of "calyear".}
#' \item{calyear}{The year corresponding to "county_code_original".}
#' \item{weighting}{The weighting that needs to be applied.}
#' \item{granularity_geo}{nation, county, municip, wardbergen, wardoslo, wardstavanger, wardtrondheim, missingwardbergen, missingwardoslo, missingwardstavanger, missingwardtrondheim, notmainlandcounty, notmainlandmunicip, missingcounty}
#' }
#' @examples
#' csdata::nor_locations_redistricting()
#' @export
nor_locations_redistricting <- function(
    border = csdata::config$border_nor
){

  stopifnot(border %in% c(2020, 2024))
  if(border == 2020){
    x <- get0("nor_locations_redistricting_b2020", envir = asNamespace("csdata"))
  } else if(border == 2024){
    x <- get0("nor_locations_redistricting_b2024", envir = asNamespace("csdata"))
  }
  d <- copy(x)
  return(d)
}

