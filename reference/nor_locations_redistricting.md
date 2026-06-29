# All redistricting in Norway

This function returns a dataset that is used to transfer "original"
datasets to the 2020 or 2024 borders.

## Usage

``` r
nor_locations_redistricting(border = csdata::config$border_nor)
```

## Arguments

- border:

  The year in which Norwegian geographical boundaries were designated
  (2020, 2024).

## Value

- location_code_current:

  The location code per today.

- location_code_original:

  The location code as of "calyear".

- calyear:

  The year corresponding to "county_code_original".

- weighting:

  The weighting that needs to be applied.

- granularity_geo:

  nation, county, municip, wardbergen, wardoslo, wardstavanger,
  wardtrondheim, missingwardbergen, missingwardoslo,
  missingwardstavanger, missingwardtrondheim, notmainlandcounty,
  notmainlandmunicip, missingcounty

## Examples

``` r
csdata::nor_locations_redistricting()
#>                 location_code_current         location_code_original calyear
#>                                <char>                         <char>   <num>
#>     1:                     nation_nor                     nation_nor    1975
#>     2:                     nation_nor                     nation_nor    1976
#>     3:                     nation_nor                     nation_nor    1977
#>     4:                     nation_nor                     nation_nor    1978
#>     5:                     nation_nor                     nation_nor    1979
#>    ---                                                                      
#> 41679: missingwardstavanger_nor110399 missingwardstavanger_nor110399    2030
#> 41680: missingwardstavanger_nor110399 missingwardstavanger_nor110399    2031
#> 41681: missingwardstavanger_nor110399 missingwardstavanger_nor110399    2032
#> 41682: missingwardstavanger_nor110399 missingwardstavanger_nor110399    2033
#> 41683: missingwardstavanger_nor110399 missingwardstavanger_nor110399    2034
#>        weighting      granularity_geo
#>            <num>               <char>
#>     1:         1               nation
#>     2:         1               nation
#>     3:         1               nation
#>     4:         1               nation
#>     5:         1               nation
#>    ---                               
#> 41679:         1 missingwardstavanger
#> 41680:         1 missingwardstavanger
#> 41681:         1 missingwardstavanger
#> 41682:         1 missingwardstavanger
#> 41683:         1 missingwardstavanger
```
