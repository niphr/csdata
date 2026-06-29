# Convert location_code to iso3

Convert location_code to iso3

## Usage

``` r
location_code_to_iso3(x)
```

## Arguments

- x:

  Either a vector, or a data.frame/data.table containing a column called
  "location_code".

## Value

Character vector the same length as x, containing the corresponding
iso3.

## Examples

``` r
csdata::location_code_to_iso3(c("nation_nor", "county_nor03"))
#> [1] "nor" "nor"
```
