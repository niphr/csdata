# Location hierarchies in Norway

Calculates the relationship between different locations in Norway,
according to geographic granularity. For example, which municipalities
are inside which counties.

## Usage

``` r
nor_locations_hierarchy_from_to(
  from,
  to,
  include_to_name = FALSE,
  border = csdata::config$border_nor
)
```

## Arguments

- from:

  wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion,
  county, georegion, mtregion, notmainlandmunicip, notmainlandcounty,
  missingmunicip, missingcounty

- to:

  wardoslo, wardbergen, wardtrondheim, wardstavanger, municip, baregion,
  county, georegion, mtregion, notmainlandmunicip, notmainlandcounty,
  missingmunicip, missingcounty

- include_to_name:

  Do you want to include the name of the 'to' location?

- border:

  The year in which Norwegian geographical boundaries were designated
  (2020, 2024).

## Value

Data.table containing the columns:

- from_code

- to_code

- to_name (if include_to_name==TRUE)

## Examples

``` r
csdata::nor_locations_hierarchy_from_to(from="wardoslo", to="county")
#>              from_code      to_code
#>                 <char>       <char>
#>  1: wardoslo_nor030101 county_nor03
#>  2: wardoslo_nor030102 county_nor03
#>  3: wardoslo_nor030103 county_nor03
#>  4: wardoslo_nor030104 county_nor03
#>  5: wardoslo_nor030105 county_nor03
#>  6: wardoslo_nor030106 county_nor03
#>  7: wardoslo_nor030107 county_nor03
#>  8: wardoslo_nor030108 county_nor03
#>  9: wardoslo_nor030109 county_nor03
#> 10: wardoslo_nor030110 county_nor03
#> 11: wardoslo_nor030111 county_nor03
#> 12: wardoslo_nor030112 county_nor03
#> 13: wardoslo_nor030113 county_nor03
#> 14: wardoslo_nor030114 county_nor03
#> 15: wardoslo_nor030115 county_nor03
csdata::nor_locations_hierarchy_from_to(from="municip", to="baregion")
#> Empty data.table (0 rows and 2 cols): from_code,to_code
```
