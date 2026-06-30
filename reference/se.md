# Swedish characters in unicode

A named list of Swedish special characters as unicode strings, for use
where literal non-ASCII characters are inconvenient. Elements: `OE`/`oe`
(Oe/oe), `AE`/`ae` (Ae/ae).

## Usage

``` r
se
```

## Examples

``` r
print(csdata::se)
#> $OE
#> [1] "Ö"
#> 
#> $oe
#> [1] "ö"
#> 
#> $AE
#> [1] "Ä"
#> 
#> $ae
#> [1] "ä"
#> 
csdata::se$oe  # lowercase oe
#> [1] "ö"
```
