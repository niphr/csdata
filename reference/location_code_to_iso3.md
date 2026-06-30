# Convert location codes to ISO 3166-1 alpha-3 country codes

Returns the ISO 3166-1 alpha-3 country code for each location code.
Currently all Norwegian location codes map to `"nor"`.

## Usage

``` r
location_code_to_iso3(x)
```

## Arguments

- x:

  A character vector of location codes, or a data.table / data.frame
  containing a column named `location_code`.

## Value

A character vector the same length as `x` (or with as many elements as
there are rows in `x` when `x` is a data.table), containing the
corresponding ISO 3166-1 alpha-3 country code (always `"nor"`).

## Examples

``` r
csdata::location_code_to_iso3(c("nation_nor", "county_nor03", "municip_nor0301"))
#> [1] "nor" "nor" "nor"

library(data.table)
dt <- data.table(location_code = c("nation_nor", "county_nor03"))
csdata::location_code_to_iso3(dt)
#> [1] "nor" "nor"
```
