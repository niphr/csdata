# Redistricting weights for Norwegian geographic units

Returns a data.table of weighting factors used to convert historical
data recorded under old administrative boundaries to the 2020 or 2024
borders. Each row maps an original location code (as of a given calendar
year) to the current location code, with a proportional weighting.

## Usage

``` r
nor_locations_redistricting(border = csdata::config$border_nor)
```

## Source

Statistics Norway (SSB) municipal reform documentation.

## Arguments

- border:

  Integer. The target geographic border year. Valid values: `2020`,
  `2024`. Defaults to `csdata::config$border_nor`.

## Value

A data.table with columns:

- location_code_current:

  Location code under the target border year.

- location_code_original:

  Location code as it existed in `calyear`.

- calyear:

  The calendar year to which `location_code_original` applies.

- weighting:

  Proportional weight to apply when aggregating from the original
  location to the current location (values sum to 1 within each
  `location_code_original` / `calyear` group).

- granularity_geo:

  Geographic granularity: one of `"nation"`, `"county"`, `"municip"`,
  `"wardbergen"`, `"wardoslo"`, `"wardstavanger"`, `"wardtrondheim"`,
  `"missingwardbergen"`, `"missingwardoslo"`, `"missingwardstavanger"`,
  `"missingwardtrondheim"`, `"notmainlandcounty"`,
  `"notmainlandmunicip"`, `"missingcounty"`.

## Examples

``` r
d <- csdata::nor_locations_redistricting()
head(d)
#>    location_code_current location_code_original calyear weighting
#>                   <char>                 <char>   <num>     <num>
#> 1:            nation_nor             nation_nor    1975         1
#> 2:            nation_nor             nation_nor    1976         1
#> 3:            nation_nor             nation_nor    1977         1
#> 4:            nation_nor             nation_nor    1978         1
#> 5:            nation_nor             nation_nor    1979         1
#> 6:            nation_nor             nation_nor    1980         1
#>    granularity_geo
#>             <char>
#> 1:          nation
#> 2:          nation
#> 3:          nation
#> 4:          nation
#> 5:          nation
#> 6:          nation
d[calyear == 2019 & granularity_geo == "municip"]
#>      location_code_current location_code_original calyear weighting
#>                     <char>                 <char>   <num>     <num>
#>   1:       municip_nor0301        municip_nor0301    2019         1
#>   2:       municip_nor1101        municip_nor1101    2019         1
#>   3:       municip_nor1103        municip_nor1103    2019         1
#>   4:       municip_nor1103        municip_nor1141    2019         1
#>   5:       municip_nor1103        municip_nor1142    2019         1
#>  ---                                                               
#> 426:       municip_nor5628        municip_nor2025    2019         1
#> 427:       municip_nor5630        municip_nor2024    2019         1
#> 428:       municip_nor5632        municip_nor2028    2019         1
#> 429:       municip_nor5634        municip_nor2002    2019         1
#> 430:       municip_nor5636        municip_nor2027    2019         1
#>      granularity_geo
#>               <char>
#>   1:         municip
#>   2:         municip
#>   3:         municip
#>   4:         municip
#>   5:         municip
#>  ---                
#> 426:         municip
#> 427:         municip
#> 428:         municip
#> 429:         municip
#> 430:         municip
```
