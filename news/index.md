# Changelog

## Version 2026.7.2

- **Breaking:** dropped support for the 2020 border year. All `_b2020`
  datasets are removed and every data-returning function now accepts
  only `border = 2024` (`set_config(border_nor = 2020)` and
  `border = 2020` now error). Use `border = 2024`.
- Population data now carries a sex dimension.
  `nor_population_by_age_b2024` includes real `male`/`female`/`total`
  splits (fetched from SSB tables 07459 and 10826 with `Kjonn = TRUE`).
  Locations where Statistics Norway provides no sex breakdown (Svalbard,
  Jan Mayen, unknown) carry `total` with `NA` male/female.
- New exported function
  [`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md)
  returns population by custom age categories split by sex, with
  `include_total_age` and `include_total_sex` toggles.
- [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
  is unchanged for callers: it now filters to `sex == "total"`
  internally, so its output remains identical (total only).
- Note: at redistricted sub-national levels each sex is rounded
  independently, so `male + female` may differ from `total` by up to 3
  people in ~0.6% of historical cells (exact at the national level).

## Version 2026.3.30

CRAN release: 2026-03-31

- Population data pipeline now fetches directly from SSB PxWeb API
  (tables 07459 and 10826) instead of manually downloaded CSV files.
- Including 2025 and 2026 population data.
- Municipality population data now uses actual historical data per
  municipality code instead of crude split approximations for merged
  municipalities.
- National population data sourced from table 07459 (1986+), replacing
  the defunct dataset 59322.
- Svalbard population data now fetched from SSB table 07430 (up to
  2026), replacing the static Excel file.
- Removed unused Nordic country population and location Excel files.

## Version 2024.9.26

- location_name_file_nb_ascii in csdata::nor_location_names is now in
  lowercase.

## Version 2024.4.26

CRAN release: 2024-04-26

- Fixing an error that wouldn’t allow for include_total = TRUE in
  nor_population_by_age_cats
- CRAN submission.

## Version 2024.4.21

- Including 2024 population data.

## Version 2024.1.17

CRAN release: 2024-01-18

- Fixing an error in merging for 2023-\>2024.
- Including population for georegion.

## Version 2023.12.26

- Improving names for georegion for border=2024.

## Version 2023.11.29

- Fixing error in redistricting of counties for border=2024.

## Version 2023.8.21

- Fixing error in the name Trøndelag-Trööndelage.

## Version 2023.7.31

- Including Sami names for counties.

## Version 2023.6.19

- Inclusion of location_name_short in nor_locations_names for
  border=2020.
- CRAN submission.

## Version 2023.5.26

- Inclusion of location_name_short in nor_locations_names for
  border=2024.

## Version 2023.5.22

CRAN release: 2023-05-22

- Including redistricting data for
  [2024](https://www.regjeringen.no/no/tema/kommuner-og-regioner/kommunestruktur/nye-kommune-og-fylkesnummer-fra-1.-januar-2024/id2924701/?expand=factbox2924711)
- Setting default for border_nor to 2024.
- CRAN submission.

## Version 2023.4.21

CRAN release: 2023-04-21

- Include population data for 2023.
