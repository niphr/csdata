# Norwegian population aggregated into custom age categories

Aggregates the bundled Norwegian population dataset (from Statistics
Norway) into caller-defined age bands. The underlying data covers every
integer age from 0 to 105 at national, county, municipality, and
city-district level.

## Usage

``` r
nor_population_by_age_cats(
  cats = NULL,
  include_total = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
)
```

## Arguments

- cats:

  A named or unnamed list of integer vectors specifying the age values
  to include in each category. Each vector element defines one age band.
  If a list element is named, that name is used as the `age` label;
  otherwise the label is auto-generated as `"LLL_HHH"` (zero-padded
  lower and upper bounds). Defaults to `NULL` (no custom bands; only
  `"total"` is returned when `include_total = TRUE`).

- include_total:

  Logical. If `TRUE` (default), an additional row group with
  `age = "total"` covering all ages is appended.

- include_9999:

  Logical. If `TRUE`, the most recent calendar year is duplicated and
  added with `calyear = 9999`, following the cstidy convention for
  `granularity_time == "event_*"`. Default `FALSE`.

- border:

  Integer. The geographic border year. Valid values: `2024`. Defaults to
  `csdata::config$border_nor`.

## Value

A data.table with columns:

- granularity_geo:

  Geographic granularity level.

- location_code:

  Location code.

- age:

  Age category label, as derived from `cats` names or auto-generated,
  plus `"total"` if `include_total = TRUE`.

- sex:

  Sex. Always `"total"` in the current dataset.

- calyear:

  Calendar year.

- pop_jan1_n:

  Population count as of 1 January of `calyear`.

- imputed:

  Logical. `TRUE` if the value was imputed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Not run: aggregates the full population dataset, which exceeds CRAN's
# example time limit.

# Default: return total population only
d <- nor_population_by_age_cats()
head(d[granularity_geo == "nation"])

# Named age bands
d2 <- nor_population_by_age_cats(
  cats = list("children" = 0:17, "working_age" = 18:66, "elderly" = 67:105),
  include_total = TRUE
)
d2[granularity_geo == "nation" & calyear == 2024]
} # }
```
