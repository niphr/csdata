# An environment containing configuration variables

Available configuration variables:

- border_nor (default 2024): The year in which Norwegian geographical
  boundaries were designated. Valid values: 2020, 2024.

## Usage

``` r
config
```

## Format

An object of class `environment` of length 1.

## Examples

``` r
print(ls(csdata::config))
#> [1] "border_nor"
for(i in names(csdata::config)){
  cat(i, ":", csdata::config[[i]], "\n")
}
#> border_nor : 2024 
```
