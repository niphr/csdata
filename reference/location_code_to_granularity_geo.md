# Convert location codes to granularity_geo

Extracts the geographic granularity label from one or more location
codes. When `location_reference` is `NULL`, the granularity comes from
the lowercase alphabetic prefix of the location code (e.g.
`"county_nor03"` -\> `"county"`). The prefix `"norge"` is the one
special case, and it gives `"nation"`. When you supply a reference
table, the granularity comes from that table.

## Usage

``` r
location_code_to_granularity_geo(x, location_reference = NULL)
```

## Arguments

- x:

  A character vector of location codes, or a data.table with a column
  named `location_code`. A plain data.frame is not supported: it falls
  through to the default method, which treats the frame itself as the
  vector of codes.

- location_reference:

  A data.table with columns `location_code` and `granularity_geo` to use
  for lookup. When `NULL` (default), granularity comes from the location
  code prefix.

## Value

A character vector of the corresponding `granularity_geo` values. It has
the same length as `x`, or as many elements as `x` has rows when `x` is
a data.table. A code with no leading lowercase letters yields `NA`. An
unmatched code also yields `NA` when you supply `location_reference`.

The length guarantee has one exception. `location_reference` is joined
on `location_code`, so a code that appears more than once in the
reference contributes one element per matching row.
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
holds one such code today, `"lab_nor084467"`, which two laboratories
share.

## See also

[`vignette("csdata", package = "csdata")`](https://niphr.github.io/csdata/articles/csdata.md),
which calls this function on a worked example.
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
