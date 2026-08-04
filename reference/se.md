# Swedish characters in unicode

A named list of Swedish special characters as unicode strings, for use
where literal non-ASCII characters are inconvenient. Elements: `OE`/`oe`
(Oe/oe), `AE`/`ae` (Ae/ae). Each element is a single character. These
four names are spelled the same way in
[nb](https://niphr.github.io/csdata/reference/nb.md), so code that
indexes `se` by name also works against `nb`.

## Usage

``` r
se
```

## See also

No vignette covers this object.

Other unicode character lists:
[`nb`](https://niphr.github.io/csdata/reference/nb.md)

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
