# Convert location_code to granularity_geo

Convert location_code to granularity_geo

## Usage

``` r
location_code_to_granularity_geo(x, location_reference = NULL)
```

## Arguments

- x:

  Either a vector, or a data.frame/data.table containing a column called
  "location_code".

- location_reference:

  A location reference data.table.

## Value

Character vector the same length as x, containing the corresponding
granularity_geo.

## Examples

``` r
csdata::location_code_to_granularity_geo(c("nation_nor", "county_nor03"))
#> [1] "nation" "county"
```
