# Location hierarchies in Norway

Returns a mapping between two geographic levels in Norway. Both `from`
and `to` accept character vectors, in which case all requested
combinations are returned combined into a single data.table.

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

  Character vector. The source geographic granularity. One or more of:
  `"wardoslo"`, `"extrawardoslo"`, `"wardbergen"`, `"wardtrondheim"`,
  `"wardstavanger"`, `"missingwardoslo"`, `"missingwardbergen"`,
  `"missingwardtrondheim"`, `"missingwardstavanger"`, `"municip"`,
  `"baregion"`, `"county"`, `"georegion"`, `"mtregion"`,
  `"notmainlandmunicip"`, `"notmainlandcounty"`, `"missingmunicip"`,
  `"missingcounty"`.

- to:

  Character vector. The target geographic granularity. Same valid values
  as `from`.

- include_to_name:

  Logical. If `TRUE`, include the name of each `to` location as a third
  column `to_name`. Default `FALSE`.

- border:

  Integer. The geographic border year. Valid values: `2020`, `2024`.
  Defaults to `csdata::config$border_nor`.

## Value

A data.table with columns:

- from_code:

  Location code at the `from` granularity level.

- to_code:

  Location code at the `to` granularity level.

- to_name:

  Name of the `to` location (only present when
  `include_to_name = TRUE`).

## Examples

``` r
csdata::nor_locations_hierarchy_from_to(from = "wardoslo", to = "county")
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
csdata::nor_locations_hierarchy_from_to(from = "municip", to = "baregion")
#> Empty data.table (0 rows and 2 cols): from_code,to_code
csdata::nor_locations_hierarchy_from_to(
  from = c("municip", "county"),
  to   = "georegion",
  include_to_name = TRUE
)
#>            from_code        to_code                  to_name
#>               <char>         <char>                   <char>
#>   1: municip_nor0301 georegion_nor5     Østlandet-Austlandet
#>   2: municip_nor1101 georegion_nor3               Vestlandet
#>   3: municip_nor1103 georegion_nor3               Vestlandet
#>   4: municip_nor1106 georegion_nor3               Vestlandet
#>   5: municip_nor1108 georegion_nor3               Vestlandet
#>  ---                                                        
#> 368:    county_nor42 georegion_nor4                    Agder
#> 369:    county_nor46 georegion_nor3               Vestlandet
#> 370:    county_nor50 georegion_nor2    Trøndelag-Trööndelage
#> 371:    county_nor55 georegion_nor1 Nord-Norge-Davvi-Norggas
#> 372:    county_nor56 georegion_nor1 Nord-Norge-Davvi-Norggas
```
