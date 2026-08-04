# Convert location codes to ISO 3166-1 alpha-3 country codes

Returns the ISO 3166-1 alpha-3 country code for each location code.
csdata carries Norwegian data only, so the implementation returns
`"nor"` for every element without inspecting its value. A code from
another country, or a string that is not a location code at all, also
returns `"nor"`.

## Usage

``` r
location_code_to_iso3(x)
```

## Arguments

- x:

  A character vector of location codes, or a data.table containing a
  column named `location_code`. A plain data.frame is not supported: it
  falls through to the default method, which returns one value per
  column rather than one per row.

## Value

A character vector of `"nor"`, the same length as `x`, or with as many
elements as there are rows in `x` when `x` is a data.table.

## See also

No vignette covers this function.
[`vignette("locations_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/locations_norway.md)
tabulates the `location_code` values returned by
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md).

Other location code converters:
[`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md)

## Examples

``` r
csdata::location_code_to_iso3(c("nation_nor", "county_nor03", "municip_nor0301"))
#> [1] "nor" "nor" "nor"

# the input value is not inspected
csdata::location_code_to_iso3(c("county_nor03", "xyz"))
#> [1] "nor" "nor"

library(data.table)
dt <- data.table(location_code = c("nation_nor", "county_nor03"))
csdata::location_code_to_iso3(dt)
#> [1] "nor" "nor"
```
