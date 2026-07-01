#' Population in Norway by sex and age categories
#'
#' A function that categorizes the Norwegian population into custom age categories,
#' split by sex (male/female) and optionally the total of both sexes.
#'
#' Real male/female splits are only available for the 2024 borders (`border = 2024`).
#' For the 2020 borders (a frozen legacy dataset) and for locations where Statistics
#' Norway provides no sex breakdown (Svalbard, Jan Mayen, unknown), `male` and `female`
#' are returned as `NA` while `total` holds the real count.
#'
#' Note that when `include_total_sex = TRUE` the output holds `male`, `female` and
#' `total` in long format, so summing `pop_jan1_n` across all sex values double-counts.
#' Filter to a single sex, or sum only the `male`/`female` components.
#'
#' @param cats A list containing vectors that you want to categorize.
#' @param include_total_age Boolean. Should 'total' be included as an age cat?
#' @param include_total_sex Boolean. Should 'total' (both sexes combined) be included
#' as a sex, alongside 'male' and 'female'?
#' @param include_9999 Boolean. Should the current year is duplicated and added as "calyear==9999".
#' This is in accordance with the cstidy principles regarding granularity_time=="event_*".
#' @param border The year in which Norwegian geographical boundaries were designated (2020, 2024).
#' @examples
#' \dontrun{
#' nor_population_by_sex_age_cats(cats = list(c(1:10), c(11:20)))
#' }
#' @return A data.table containing the following columns:
#' - granularity_geo
#' - location_code
#' - age (as specified in the argument "cats")
#' - sex ("male", "female", and "total" if include_total_sex)
#' - calyear
#' - pop_jan1_n
#' - imputed
#' @export
nor_population_by_sex_age_cats <- function(
  cats = NULL,
  include_total_age = TRUE,
  include_total_sex = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
){

  calyear <- sex <- NULL
  if(is.null(cats)) cats <- list()
  stopifnot(is.list(cats))
  stopifnot(border %in% c(2020, 2024))

  if(border==2020){
    x <- get0("nor_population_by_age_b2020", envir = asNamespace("csdata"))
  } else if(border==2024){
    x <- get0("nor_population_by_age_b2024", envir = asNamespace("csdata"))
  }
  data <- copy(x)

  if(!include_total_sex){
    data <- data[sex != "total"]
  }

  if(include_total_age){
    cats[[length(cats)+1]] <- -99:1000
  }

  retval <- vector("list", length = length(cats))
  for(i in seq_along(cats)){
    vals <- cats[[i]]
    name <- names(cats)[i]
    if(include_total_age & i==length(cats)){
      name <- "total"
    } else if(is.null(name)){
      name <- paste0(formatC(vals[1],width=3,flag="0"),"_",formatC(vals[length(vals)],width=3,flag="0"))
    } else if(is.na(name) | name==""){
      name <- paste0(formatC(vals[1],width=3,flag="0"),"_",formatC(vals[length(vals)],width=3,flag="0"))
    }

    retval[[i]] <- nor_population_by_age_internal(
      data,
      vals = vals,
      name = name
    )
  }

  retval <- rbindlist(retval)

  # 9999 as current year
  if(include_9999){
    x <- retval[calyear==format.Date(Sys.time(),"%Y")]
    x[, calyear := 9999]
    retval <- rbindlist(list(retval, x), use.names = T)
  }

  return(retval)
}
