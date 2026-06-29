# Population in Norway by categories

A function that easily categorizes the Norwegian population into
different age categories.

## Usage

``` r
nor_population_by_age_cats(
  cats = NULL,
  include_total = TRUE,
  include_9999 = FALSE,
  border = csdata::config$border_nor
)
```

## Arguments

- cats:

  A list containing vectors that you want to categorize.

- include_total:

  Boolean. Should 'total' be included as an age cat?

- include_9999:

  Boolean. Should the current year is duplicated and added as
  "calyear==9999". This is in accordance with the cstidy principles
  regarding granularity_time=="event\_\*".

- border:

  The year in which Norwegian geographical boundaries were designated
  (2020, 2024).

## Value

A data.table containing the following columns:

- granularity_geo

- location_code

- age (as specified in the argument "cats")

- sex ("total")

- calyear

- pop_jan1_n

- imputed

## Examples

``` r
if (FALSE) { # \dontrun{
nor_population_by_age_cats(cats = list(c(1:10), c(11:20)))
} # }
```
