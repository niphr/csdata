# Set package configuration options

Updates `border_nor` in the
[config](https://niphr.github.io/csdata/reference/config.md)
environment. Call this at the start of a script to change the default
`border` argument of
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md),
[`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md),
[`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md),
[`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
and
[`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md).

## Usage

``` r
set_config(border_nor = NULL)
```

## Arguments

- border_nor:

  Integer. The Norwegian geographic border year to use as the default.
  Valid values: `2024`. Pass `NULL` to leave unchanged.

## Value

Invisibly, the value assigned to `config$border_nor`, or `NULL` when
`border_nor` is `NULL`. The function exists for its side effect on
[config](https://niphr.github.io/csdata/reference/config.md).

## See also

No vignette covers this function.

## Examples

``` r
old <- csdata::config$border_nor
csdata::set_config(border_nor = 2024)
csdata::config$border_nor  # 2024
#> [1] 2024
csdata::set_config(border_nor = old)  # restore
```
