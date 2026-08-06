# Add an iso3 column to a data set

Adds an `iso3` column that holds the ISO 3166-1 alpha-3 country code,
and changes `x` in place. csdata carries Norwegian data only, so the
value is `"nor"` on every row. The function does not read the contents
of `x`, so `x` gains the column even when it has no `location_code`
column.

## Usage

``` r
add_iso3_to_data_set(x)
```

## Arguments

- x:

  A data.table. Only a data.table method exists, so any other class
  raises "no applicable method".

## Value

`x`, invisibly, with a new `iso3` column holding `"nor"` on every row.

## See also

No vignette covers this function.
[`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md)
returns the same values as a vector instead of writing them onto `x`.

Other data set column adders:
[`add_granularity_geo_to_data_set()`](https://niphr.github.io/csdata/reference/add_granularity_geo_to_data_set.md)

## Examples

``` r
library(data.table)
data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
csdata::add_iso3_to_data_set(data)
print(data)
#>    location_code   iso3
#>           <char> <char>
#> 1:    nation_nor    nor
#> 2:  county_nor03    nor
#> 3:          blah    nor
```
