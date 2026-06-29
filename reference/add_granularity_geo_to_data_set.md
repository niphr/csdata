# Adds granularity_geo to a given data set

Adds granularity_geo to a given data set

## Usage

``` r
add_granularity_geo_to_data_set(x, location_reference = NULL)
```

## Arguments

- x:

  A data.table containing a column called "location_code".

- location_reference:

  A location reference data.table.

## Value

A data.table containing an extra column called "granularity_geo".

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
data <- data.table(location_code = c("norge", "county03", "blah"))
csdata::add_granularity_geo_to_data_set(data)
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:         norge          nation
#> 2:      county03          county
#> 3:          blah            blah

library(data.table)
data <- data.table(location_code = c("norge", "county03", "blah"))
csdata::add_granularity_geo_to_data_set(data, location_reference = csdata::nor_locations_names())
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:         norge            <NA>
#> 2:      county03            <NA>
#> 3:          blah            <NA>
```
