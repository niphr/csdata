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

  A character vector of location codes, or a data.table containing a
  column named `location_code`. A plain data.frame is not supported: it
  falls through to the default method, which treats the frame itself as
  the vector of codes.

- location_reference:

  A data.table with columns `location_code` and `granularity_geo` to use
  for lookup. When `NULL` (default), granularity is inferred from the
  location code prefix.

## Value

A character vector the same length as `x`, or with as many elements as
there are rows in `x` when `x` is a data.table, containing the
corresponding `granularity_geo` values. A code with no leading lowercase
letters yields `NA`, and an unmatched code yields `NA` when
`location_reference` is supplied.

The length guarantee has one exception. `location_reference` is joined
on `location_code`, so a code that appears more than once in the
reference contributes one element per matching row.
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
holds one such code today, `"lab_nor084467"`, which two laboratories
share.

## See also

No vignette covers this function.
[`vignette("locations_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/locations_norway.md)
tabulates the `location_code` values returned by
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md).

Other location code converters:
[`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md)

## Examples

``` r
csdata::location_code_to_granularity_geo(c("nation_nor", "county_nor03", "municip_nor0301"))
#> [1] "nation"  "county"  "municip"

# a code with no lowercase prefix gives NA
csdata::location_code_to_granularity_geo(c("nation_nor", "", NA))
#> [1] "nation" NA       NA      

library(data.table)
dt <- data.table(location_code = c("nation_nor", "county_nor03"))
csdata::location_code_to_granularity_geo(dt)
#> [1] "nation" "county"

# looked up against a reference table instead of parsed from the prefix
csdata::location_code_to_granularity_geo(
  c("nation_nor", "county_nor03", "blah"),
  location_reference = csdata::nor_locations_names()
)
#> [1] "nation" "county" NA      
```
