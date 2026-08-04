# Norwegian characters in unicode

A named list of Norwegian special characters as unicode strings, for use
where literal non-ASCII characters are inconvenient. Elements: `AA`/`aa`
(Aa/aa), `OE`/`oe` (Oe/oe), `AE`/`ae` (Ae/ae). Each element is a single
character.

## Usage

``` r
nb
```

## See also

No vignette covers this object.

Other unicode character lists:
[`se`](https://niphr.github.io/csdata/reference/se.md)

## Examples

``` r
print(csdata::nb)
#> $AA
#> [1] "Å"
#> 
#> $aa
#> [1] "å"
#> 
#> $OE
#> [1] "Ø"
#> 
#> $oe
#> [1] "ø"
#> 
#> $AE
#> [1] "Æ"
#> 
#> $ae
#> [1] "æ"
#> 
csdata::nb$AA  # uppercase Aa
#> [1] "Å"
```
