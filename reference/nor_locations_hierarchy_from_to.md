# Location hierarchies in Norway

Returns a mapping between two geographic levels in Norway. Both `from`
and `to` accept character vectors. The function then returns every
requested combination, joined into a single data.table.

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

  Integer. The geographic border year. Valid values: `2024`. Defaults to
  `csdata::config$border_nor`.

## Value

A data.table with columns:

- from_code:

  Location code at the `from` granularity level.

- to_code:

  Location code at the `to` granularity level.

- to_name:

  Name of the `to` location (only present when
  `include_to_name = TRUE`).

The table has no key set, and a combination the bundled hierarchy cannot
express yields zero rows.

## Details

A combination that the bundled hierarchy table cannot express returns a
zero-row data.table rather than an error. A `from` or `to` value of
`"baregion"` always does this, because the bundled table carries no
BA-region codes. Use
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
to list the BA-regions themselves.

## See also

[`vignette("csdata", package = "csdata")`](https://niphr.github.io/csdata/articles/csdata.md),
which calls `nor_locations_hierarchy_from_to()` on a worked example.
[`vignette("locations_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/locations_norway.md)
tabulates the `location_code` values returned by
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md),
which are the values `from_code` and `to_code` are drawn from.

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

nrow(csdata::nor_locations_hierarchy_from_to(from = "municip", to = "county"))
#> [1] 357

csdata::nor_locations_hierarchy_from_to(
  from = c("municip", "county"),
  to   = "georegion",
  include_to_name = TRUE
)[1:5]
#>          from_code        to_code              to_name
#>             <char>         <char>               <char>
#> 1: municip_nor0301 georegion_nor5 Østlandet-Austlandet
#> 2: municip_nor1101 georegion_nor3           Vestlandet
#> 3: municip_nor1103 georegion_nor3           Vestlandet
#> 4: municip_nor1106 georegion_nor3           Vestlandet
#> 5: municip_nor1108 georegion_nor3           Vestlandet

# baregion is accepted but the bundled hierarchy holds no BA-region codes
nrow(csdata::nor_locations_hierarchy_from_to(from = "municip", to = "baregion"))
#> [1] 0
```
