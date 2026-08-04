# Norwegian population aggregated into custom age categories

Aggregates the bundled Norwegian population dataset (from Statistics
Norway) into caller-defined age bands. The underlying data covers every
integer age from 0 to 105 at national, georegion, county, municipality,
and city-district level. Not-mainland and unknown locations carry the
single placeholder age `-99` instead, which `include_total` counts but a
band such as `0:105` does not.

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

  Sex. Always `"total"`, because this function filters the bundled
  dataset to the sex total. The dataset itself also holds `"male"` and
  `"female"`; reach those with
  [`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md).

- calyear:

  Calendar year.

- pop_jan1_n:

  Population count as of 1 January of `calyear`.

- imputed:

  Logical. `TRUE` if the value was imputed.

## See also

[`vignette("population_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/population_norway.md)
plots the national series and tabulates `pop_jan1_n` by location and
calendar year from this function.
[`vignette("csdata", package = "csdata")`](https://niphr.github.io/csdata/articles/csdata.md)
gives the csverse coding rules for the `age` and `sex` columns.

Other population data:
[`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md)

## Examples

``` r
# data.table's default multi-threading pushes this example past CRAN's
# CPU-to-elapsed limit, so pin it to one thread and restore afterwards.
old_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)

# Default: return total population only
d <- nor_population_by_age_cats()
print(head(d[granularity_geo == "nation"]))
#>    granularity_geo location_code    age    sex calyear pop_jan1_n imputed
#>             <char>        <char> <char> <char>   <num>      <num>  <lgcl>
#> 1:          nation    nation_nor  total  total    1986    4159187   FALSE
#> 2:          nation    nation_nor  total  total    1987    4175521   FALSE
#> 3:          nation    nation_nor  total  total    1988    4198289   FALSE
#> 4:          nation    nation_nor  total  total    1989    4220686   FALSE
#> 5:          nation    nation_nor  total  total    1990    4233116   FALSE
#> 6:          nation    nation_nor  total  total    1991    4249830   FALSE

# Named age bands
d2 <- nor_population_by_age_cats(
  cats = list("children" = 0:17, "working_age" = 18:66, "elderly" = 67:105),
  include_total = TRUE
)
print(d2[granularity_geo == "nation" & calyear == 2024])
#>    granularity_geo location_code         age    sex calyear pop_jan1_n imputed
#>             <char>        <char>      <char> <char>   <num>      <num>  <lgcl>
#> 1:          nation    nation_nor    children  total    2024    1112853   FALSE
#> 2:          nation    nation_nor working_age  total    2024    3521869   FALSE
#> 3:          nation    nation_nor     elderly  total    2024     915481   FALSE
#> 4:          nation    nation_nor       total  total    2024    5550203   FALSE

data.table::setDTthreads(old_threads)
```
