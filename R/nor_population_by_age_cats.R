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
#' from 0 to 105 at national, georegion, county, municipality, and
#' city-district level. Not-mainland and unknown locations carry the single
#' placeholder age `-99` instead. `include_total` counts that age, and a band
#' such as `0:105` does not.
#'
#' @param cats A named or unnamed list of integer vectors that give the age
#'   values to include in each category. Each vector element defines one age
#'   band. If a list element is named, that name becomes the `age` label. If it
#'   is not named, the label is `"LLL_HHH"`: the zero-padded lower and upper
#'   bounds. Defaults to `NULL` (no custom bands; only `"total"` is
#'   returned when `include_total = TRUE`).
#' @param include_total Logical. If `TRUE` (default), the function appends an
#'   extra row group with `age = "total"` that covers all ages.
#' @param include_9999 Logical. If `TRUE`, the function duplicates the most
#'   recent calendar year and adds it with `calyear = 9999`. This follows the
#'   cstidy convention for `granularity_time == "event_*"`. Default `FALSE`.
#' @param border Integer. The geographic border year. Valid values: `2024`.
#'   Defaults to `csdata::config$border_nor`.
#' @returns A data.table with columns:
#'   \describe{
#'     \item{granularity_geo}{Geographic granularity level.}
#'     \item{location_code}{Location code.}
#'     \item{age}{Age category label, as derived from `cats` names or
#'       auto-generated, plus `"total"` if `include_total = TRUE`.}
#'     \item{sex}{Sex. Always `"total"`, because this function filters the
#'       bundled dataset to the sex total. The dataset itself also holds
#'       `"male"` and `"female"`; reach those with
#'       [nor_population_by_sex_age_cats()].}
#'     \item{calyear}{Calendar year.}
#'     \item{pop_jan1_n}{Population count as of 1 January of `calyear`.}
#'     \item{imputed}{Logical. `TRUE` if the value was imputed.}
#'   }
#' @family population data
#' @seealso \code{vignette("population_norway", package = "csdata")} plots the
#'   national series and tabulates `pop_jan1_n` by location and calendar year
#'   from this function. \code{vignette("csdata", package = "csdata")} gives the
#'   csverse coding rules for the `age` and `sex` columns.
#' @examples
#' # data.table's default multi-threading pushes this example past CRAN's
#' # CPU-to-elapsed limit, so pin it to one thread and restore afterwards.
#' old_threads <- data.table::getDTthreads()
#' data.table::setDTthreads(1)
#'
#' # Default: return total population only
#' d <- nor_population_by_age_cats()
#' print(head(d[granularity_geo == "nation"]))
#'
#' # Named age bands
#' d2 <- nor_population_by_age_cats(
#'   cats = list("children" = 0:17, "working_age" = 18:66, "elderly" = 67:105),
#'   include_total = TRUE
#' )
#' print(d2[granularity_geo == "nation" & calyear == 2024])
#'
#' data.table::setDTthreads(old_threads)
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
