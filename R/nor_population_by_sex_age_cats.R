#' Population in Norway by sex and age categories
#'
#' Aggregates the Norwegian population into custom age categories, split by
#' sex (male/female) and optionally the total of both sexes.
#'
#' Statistics Norway provides no sex breakdown for Svalbard and Jan Mayen
#' (`notmainlandcounty_nor21`, `notmainlandcounty_nor22`,
#' `notmainlandmunicip_nor2100`, `notmainlandmunicip_nor2200`). On those four
#' codes `pop_jan1_n` is `NA` on the `male` and `female` rows, and the `total`
#' row holds the real count. For the unknown-location codes
#' (`missingcounty_nor99`, `missingmunicip_nor9999`) `pop_jan1_n` is `NA` on all
#' three sex rows, `total` included.
#'
#' When `include_total_sex = TRUE`, the output holds `male`, `female` and
#' `total` in long format. A sum of `pop_jan1_n` across all sex values
#' therefore double-counts. Filter to a single sex, or sum only the
#' `male`/`female` components.
#'
#' @param cats A list of vectors that give the age values to include in each
#'   category.
#' @param include_total_age Logical. If `TRUE`, the function includes `"total"`
#'   as an age category.
#' @param include_total_sex Logical. If `TRUE`, the function includes `"total"`
#'   (both sexes combined) as a sex, alongside `"male"` and `"female"`.
#' @param include_9999 Logical. If `TRUE`, the function duplicates the current
#'   calendar year and adds it with `calyear = 9999`. This follows the cstidy
#'   convention for `granularity_time == "event_*"`.
#' @param border Integer. The Norwegian geographic border year. Valid values:
#'   `2024`.
#' @family population data
#' @seealso No vignette covers this function.
#'   \code{vignette("population_norway", package = "csdata")} uses
#'   [nor_population_by_age_cats()], which returns the sex total only.
#'   \code{vignette("csdata", package = "csdata")} gives the csverse coding
#'   rules for the `age` and `sex` columns.
#' @examples
#' # data.table's default multi-threading pushes this example past CRAN's
#' # CPU-to-elapsed limit, so pin it to one thread and restore afterwards.
#' old_threads <- data.table::getDTthreads()
#' data.table::setDTthreads(1)
#'
#' d <- nor_population_by_sex_age_cats(cats = list(c(1:10), c(11:20)))
#' print(d[location_code == "nation_nor" & calyear == 2024])
#'
#' data.table::setDTthreads(old_threads)
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
) {
  calyear <- sex <- NULL
  if (is.null(cats)) {
    cats <- list()
  }
  stopifnot(is.list(cats))
  stopifnot(border == 2024)

  x <- get0("nor_population_by_age_b2024", envir = asNamespace("csdata"))
  data <- copy(x)

  if (!include_total_sex) {
    data <- data[sex != "total"]
  }

  if (include_total_age) {
    cats[[length(cats) + 1]] <- -99:1000
  }

  retval <- vector("list", length = length(cats))
  for (i in seq_along(cats)) {
    vals <- cats[[i]]
    name <- names(cats)[i]
    if (include_total_age & i == length(cats)) {
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
