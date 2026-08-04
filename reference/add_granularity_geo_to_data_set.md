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

  A data.table containing a column named `location_code`. Only a
  data.table method exists, so any other class raises "no applicable
  method".

- location_reference:

  A data.table with columns `location_code` and `granularity_geo` to use
  for lookup. When `NULL` (default), granularity is derived from the
  location code prefix (e.g. `"county_nor03"` -\> `"county"`). Its
  `location_code` values should be unique. A code that appears twice in
  the reference produces more labels than `x` has rows, and the
  assignment then fails with "Supplied N items to be assigned to M items
  of column 'granularity_geo'".
  [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
  holds one such code today, `"lab_nor084467"`, which two laboratories
  share.

## Value

`x`, invisibly, with the `granularity_geo` column added or updated.

## See also

No vignette covers this function.
[`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md)
returns the same labels as a vector instead of writing them onto `x`.

Other data set column adders:
[`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md)

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
