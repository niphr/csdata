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
  Norwegian geographic datasets. Valid values: `2020`, `2024`.

## Examples

``` r
print(ls(csdata::config))
#> [1] "border_nor"
for(i in names(csdata::config)){
  cat(i, ":", csdata::config[[i]], "\n")
}
#> border_nor : 2024 
```
