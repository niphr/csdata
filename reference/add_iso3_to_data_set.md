# Add an iso3 column to a data set

Derives the ISO 3166-1 alpha-3 country code from the `location_code`
column and adds it as a new column, modifying `x` in place. Currently
all Norwegian location codes map to `"nor"`.

## Usage

``` r
add_iso3_to_data_set(x)
```

## Arguments

- x:

  A data.table containing a column named `location_code`.

## Value

`x`, invisibly, with a new `granularity_geo` column containing the ISO
3166-1 alpha-3 country code (always `"nor"` for Norwegian locations).

## Examples

``` r
library(data.table)
data <- data.table(location_code = c("nation_nor", "county_nor03", "blah"))
csdata::add_iso3_to_data_set(data)
print(data)
#>    location_code granularity_geo
#>           <char>          <char>
#> 1:    nation_nor             nor
#> 2:  county_nor03             nor
#> 3:          blah             nor
```
