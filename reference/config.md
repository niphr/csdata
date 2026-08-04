# Package configuration environment

An environment that stores package-level configuration variables. Modify
via
[`set_config()`](https://niphr.github.io/csdata/reference/set_config.md);
read directly (e.g. `config$border_nor`).

## Usage

``` r
config
```

## Details

Available variables:

- `border_nor` (default `2024`): the border year used when selecting
  Norwegian geographic datasets. Valid values: `2024`.

`border_nor` supplies the default `border` argument of
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md),
[`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md),
[`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md),
[`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
and
[`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md).
No other exported function reads it.

## See also

No vignette covers this object.

## Examples

``` r
print(ls(csdata::config))
#> [1] "border_nor"
for(i in names(csdata::config)){
  cat(i, ":", csdata::config[[i]], "\n")
}
#> border_nor : 2024 
```
