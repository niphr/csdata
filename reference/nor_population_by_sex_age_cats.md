# Population in Norway by sex and age categories

Aggregates the Norwegian population into custom age categories, split by
sex (male/female) and optionally the total of both sexes.

## Usage

``` r
nor_population_by_sex_age_cats(
  cats = NULL,
  include_total_age = TRUE,
  include_total_sex = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
)
```

## Arguments

- cats:

  A list of vectors that give the age values to include in each
  category.

- include_total_age:

  Logical. If `TRUE`, the function includes `"total"` as an age
  category.

- include_total_sex:

  Logical. If `TRUE`, the function includes `"total"` (both sexes
  combined) as a sex, alongside `"male"` and `"female"`.

- include_9999:

  Logical. If `TRUE`, the function duplicates the current calendar year
  and adds it with `calyear = 9999`. This follows the cstidy convention
  for `granularity_time == "event_*"`.

- border:

  Integer. The Norwegian geographic border year. Valid values: `2024`.

## Value

A data.table containing the following columns:

- granularity_geo

- location_code

- age (as specified in the argument "cats")

- sex ("male", "female", and "total" if include_total_sex)

- calyear

- pop_jan1_n

- imputed

## Details

Statistics Norway provides no sex breakdown for Svalbard and Jan Mayen
(`notmainlandcounty_nor21`, `notmainlandcounty_nor22`,
`notmainlandmunicip_nor2100`, `notmainlandmunicip_nor2200`). On those
four codes `pop_jan1_n` is `NA` on the `male` and `female` rows, and the
`total` row holds the real count. For the unknown-location codes
(`missingcounty_nor99`, `missingmunicip_nor9999`) `pop_jan1_n` is `NA`
on all three sex rows, `total` included.

When `include_total_sex = TRUE`, the output holds `male`, `female` and
`total` in long format. A sum of `pop_jan1_n` across all sex values
therefore double-counts. Filter to a single sex, or sum only the
`male`/`female` components.

## See also

No vignette covers this function.
[`vignette("population_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/population_norway.md)
uses
[`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md),
which returns the sex total only.
[`vignette("csdata", package = "csdata")`](https://niphr.github.io/csdata/articles/csdata.md)
gives the csverse coding rules for the `age` and `sex` columns.

Other population data:
[`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)

## Examples

``` r
# data.table's default multi-threading pushes this example past CRAN's
# CPU-to-elapsed limit, so pin it to one thread and restore afterwards.
old_threads <- data.table::getDTthreads()
data.table::setDTthreads(1)

d <- nor_population_by_sex_age_cats(cats = list(c(1:10), c(11:20)))
print(d[location_code == "nation_nor" & calyear == 2024])
#>    granularity_geo location_code     age    sex calyear pop_jan1_n imputed
#>             <char>        <char>  <char> <char>   <num>      <num>  <lgcl>
#> 1:          nation    nation_nor 001_010 female    2024     287489   FALSE
#> 2:          nation    nation_nor 001_010   male    2024     303802   FALSE
#> 3:          nation    nation_nor 001_010  total    2024     591291   FALSE
#> 4:          nation    nation_nor 011_020 female    2024     323410   FALSE
#> 5:          nation    nation_nor 011_020   male    2024     342342   FALSE
#> 6:          nation    nation_nor 011_020  total    2024     665752   FALSE
#> 7:          nation    nation_nor   total female    2024    2754485   FALSE
#> 8:          nation    nation_nor   total   male    2024    2795718   FALSE
#> 9:          nation    nation_nor   total  total    2024    5550203   FALSE

data.table::setDTthreads(old_threads)
```
