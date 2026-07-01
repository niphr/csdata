# Set package configuration options

Updates one or more variables in the
[config](https://niphr.github.io/csdata/reference/config.md)
environment. Call this at the start of a script to change the default
border year used by all data-returning functions.

## Usage

``` r
set_config(border_nor = NULL)
```

## Arguments

- border_nor:

  Integer. The Norwegian geographic border year to use as the default.
  Valid values: `2024`. Pass `NULL` to leave unchanged.

## Value

`NULL`, invisibly. Called for the side effect of updating
[config](https://niphr.github.io/csdata/reference/config.md).

## Examples

``` r
old <- csdata::config$border_nor
csdata::set_config(border_nor = 2024)
csdata::config$border_nor  # 2024
#> [1] 2024
csdata::set_config(border_nor = old)  # restore
```
