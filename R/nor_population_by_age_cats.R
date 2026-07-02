nor_population_by_age_internal <- function(
  data,
  vals,
  name
) {
  . <- age <- age_cat <- pop_jan1_n <- calyear <- location_code <- sex <- imputed <- granularity_geo <- NULL

  d <- copy(data[age %in% vals])
  d[, age_cat := name]

  d <- d[
    !is.na(age_cat),
    .(
      pop_jan1_n = sum(pop_jan1_n)
    ),
    keyby = .(
      calyear,
      location_code,
      age = age_cat,
      sex,
      imputed,
      granularity_geo
    )
  ]
  setcolorder(
    d,
    c(
      "granularity_geo",
      "location_code",
      "age",
      "sex",
      "calyear",
      "pop_jan1_n",
      "imputed"
    )
  )

  return(d)
}

#' Norwegian population aggregated into custom age categories
#'
#' Aggregates the bundled Norwegian population dataset (from Statistics Norway)
#' into caller-defined age bands. The underlying data covers every integer age
#' from 0 to 105 at national, county, municipality, and city-district level.
#'
#' @param cats A named or unnamed list of integer vectors specifying the age
#'   values to include in each category. Each vector element defines one age
#'   band. If a list element is named, that name is used as the `age` label;
#'   otherwise the label is auto-generated as `"LLL_HHH"` (zero-padded lower
#'   and upper bounds). Defaults to `NULL` (no custom bands; only `"total"` is
#'   returned when `include_total = TRUE`).
#' @param include_total Logical. If `TRUE` (default), an additional row group
#'   with `age = "total"` covering all ages is appended.
#' @param include_9999 Logical. If `TRUE`, the most recent calendar year is
#'   duplicated and added with `calyear = 9999`, following the cstidy
#'   convention for `granularity_time == "event_*"`. Default `FALSE`.
#' @param border Integer. The geographic border year. Valid values: `2024`.
#'   Defaults to `csdata::config$border_nor`.
#' @returns A data.table with columns:
#'   \describe{
#'     \item{granularity_geo}{Geographic granularity level.}
#'     \item{location_code}{Location code.}
#'     \item{age}{Age category label, as derived from `cats` names or
#'       auto-generated, plus `"total"` if `include_total = TRUE`.}
#'     \item{sex}{Sex. Always `"total"` in the current dataset.}
#'     \item{calyear}{Calendar year.}
#'     \item{pop_jan1_n}{Population count as of 1 January of `calyear`.}
#'     \item{imputed}{Logical. `TRUE` if the value was imputed.}
#'   }
#' @examples
#' \dontrun{
#' # Not run: aggregates the full population dataset, which exceeds CRAN's
#' # example time limit.
#'
#' # Default: return total population only
#' d <- nor_population_by_age_cats()
#' head(d[granularity_geo == "nation"])
#'
#' # Named age bands
#' d2 <- nor_population_by_age_cats(
#'   cats = list("children" = 0:17, "working_age" = 18:66, "elderly" = 67:105),
#'   include_total = TRUE
#' )
#' d2[granularity_geo == "nation" & calyear == 2024]
#' }
#' @export
nor_population_by_age_cats <- function(
  cats = NULL,
  include_total = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
) {
  calyear <- NULL
  sex <- NULL
  if (is.null(cats)) {
    cats <- list()
  }
  stopifnot(is.list(cats))
  stopifnot(border == 2024)

  x <- get0("nor_population_by_age_b2024", envir = asNamespace("csdata"))
  # This function is sex-agnostic and returns totals only; sex-specific breakdowns
  # are available via nor_population_by_sex_age_cats().
  data <- copy(x[sex == "total"])

  if (include_total) {
    cats[[length(cats) + 1]] <- -99:1000
  }

  retval <- vector("list", length = length(cats))
  for (i in seq_along(cats)) {
    vals <- cats[[i]]
    name <- names(cats)[i]
    if (include_total & i == length(cats)) {
      name <- "total"
    } else if (is.null(name)) {
      name <- paste0(
        formatC(vals[1], width = 3, flag = "0"),
        "_",
        formatC(vals[length(vals)], width = 3, flag = "0")
      )
    } else if (is.na(name) | name == "") {
      name <- paste0(
        formatC(vals[1], width = 3, flag = "0"),
        "_",
        formatC(vals[length(vals)], width = 3, flag = "0")
      )
    }

    retval[[i]] <- nor_population_by_age_internal(
      data,
      vals = vals,
      name = name
    )
  }

  retval <- rbindlist(retval)

  # 9999 as current year
  if (include_9999) {
    x <- retval[calyear == format.Date(Sys.time(), "%Y")]
    x[, calyear := 9999]
    retval <- rbindlist(list(retval, x), use.names = T)
  }

  return(retval)
}
