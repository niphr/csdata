# Convert location codes to granularity_geo

Extracts the geographic granularity label from one or more location
codes. When `location_reference` is `NULL`, the granularity is derived
from the lowercase alphabetic prefix of the location code (e.g.
`"county_nor03"` -\> `"county"`); the special prefix `"norge"` is mapped
to `"nation"`. When a reference table is supplied, the granularity is
looked up directly.

## Usage

``` r
location_code_to_granularity_geo(x, location_reference = NULL)
```

## Arguments

- x:

  A character vector of location codes, or a data.table / data.frame
  containing a column named `location_code`.

- location_reference:

  A data.table with columns `location_code` and `granularity_geo` to use
  for lookup. When `NULL` (default), granularity is inferred from the
  location code prefix.

## Value

A character vector the same length as `x` (or with as many elements as
there are rows in `x` when `x` is a data.table), containing the
corresponding `granularity_geo` values.

## Examples

``` r
csdata::location_code_to_granularity_geo(c("nation_nor", "county_nor03", "municip_nor0301"))
#> [1] "nation"  "county"  "municip"

library(data.table)
dt <- data.table(location_code = c("nation_nor", "county_nor03"))
csdata::location_code_to_granularity_geo(dt)
#> [1] "nation" "county"
```
