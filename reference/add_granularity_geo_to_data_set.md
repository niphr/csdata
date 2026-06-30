# Add a granularity_geo column to a data set

Derives the geographic granularity label from the `location_code` column
and adds it as a new `granularity_geo` column, modifying `x` in place.
When `location_reference` is `NULL` the granularity is inferred from the
location code prefix; when a reference table is supplied, it is looked
up directly.

## Usage

``` r
add_granularity_geo_to_data_set(x, location_reference = NULL)
```

## Arguments

- x:

  A data.table containing a column named `location_code`.

- location_reference:

  A data.table with columns `location_code` and `granularity_geo` to use
  for lookup. When `NULL` (default), granularity is derived from the
  location code prefix (e.g. `"county_nor03"` -\> `"county"`).

## Value

`x`, invisibly, with the `granularity_geo` column added or updated.

## Examples

``` r
library(data.table)
#> 
#> Attaching package: ‘data.table’
#> The following object is masked from ‘package:base’:
#> 
#>     %notin%
data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
csdata::add_granularity_geo_to_data_set(data)
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:    nation_nor          nation
#> 2:  county_nor03          county
#> 3:          blah            blah

library(data.table)
data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
csdata::add_granularity_geo_to_data_set(data, location_reference = csdata::nor_locations_names())
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:    nation_nor          nation
#> 2:  county_nor03          county
#> 3:          blah            <NA>
```
