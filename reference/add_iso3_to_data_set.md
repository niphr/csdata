# Adds iso3 to a given data set

Adds iso3 to a given data set

## Usage

``` r
add_iso3_to_data_set(x)
```

## Arguments

- x:

  A data.table containing a column called "location_code".

## Value

A data.table containing an extra column called "iso3".

## Examples

``` r
library(data.table)
data <- data.table(location_code = c("norge", "county03", "blah"))
csdata::add_iso3_to_data_set(data)
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:         norge             nor
#> 2:      county03             nor
#> 3:          blah             nor
```
