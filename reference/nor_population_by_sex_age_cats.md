# Population in Norway by sex and age categories

A function that categorizes the Norwegian population into custom age
categories, split by sex (male/female) and optionally the total of both
sexes.

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

  A list containing vectors that you want to categorize.

- include_total_age:

  Boolean. Should 'total' be included as an age cat?

- include_total_sex:

  Boolean. Should 'total' (both sexes combined) be included as a sex,
  alongside 'male' and 'female'?

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

- sex ("male", "female", and "total" if include_total_sex)

- calyear

- pop_jan1_n

- imputed

## Details

Real male/female splits are only available for the 2024 borders
(`border = 2024`). For the 2020 borders (a frozen legacy dataset) and
for locations where Statistics Norway provides no sex breakdown
(Svalbard, Jan Mayen, unknown), `male` and `female` are returned as `NA`
while `total` holds the real count.

Note that when `include_total_sex = TRUE` the output holds `male`,
`female` and `total` in long format, so summing `pop_jan1_n` across all
sex values double-counts. Filter to a single sex, or sum only the
`male`/`female` components.

## Examples

``` r
if (FALSE) { # \dontrun{
nor_population_by_sex_age_cats(cats = list(c(1:10), c(11:20)))
} # }
```
