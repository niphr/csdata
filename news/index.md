# Changelog

## Version 2026.8.6

### Licensing

- The copyright holder is now **Folkehelseinstituttet**. It read “Core
  Surveillance”, which names the package family rather than a legal
  entity.

- `DESCRIPTION` `Authors@R` now declares that holder with
  `role = "cph"`. It declared no copyright holder at all, and neither
  did any other package in the fleet. Nothing in `R CMD check` reports
  that.

- The copyright year is now 2026. It read 2023.

- `CLAUDE.md` now carries a Licensing section, so the year gets checked
  rather than silently ageing.

- Added a get-started overview to the front of
  [`vignette("csdata")`](https://niphr.github.io/csdata/articles/csdata.md),
  which pkgdown promotes to “Get started”. The page opened on
  `## Location` and a function call, and never said what the package is
  for. The new front matter states the purpose and names the five
  functions that do most of the work. It also points on to the
  `locations_norway` and `population_norway` vignettes.

- The overview also states two limitations a user meets early, each with
  the output that demonstrates it:

  - [`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md)
    returns zero rows for every `from`/`to` pair that names
    `"baregion"`. `baregion_code` and `baregion_name` are `NA` on all
    751 rows of the bundled hierarchy table, although
    [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
    lists 159 BA-regions.
  - Two laboratories share the code `"lab_nor084467"`, the only
    `location_code` that
    [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
    repeats. Pass that table as `location_reference` and
    [`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md)
    returns 4 elements for a 3-element input.
    [`add_granularity_geo_to_data_set()`](https://niphr.github.io/csdata/reference/add_granularity_geo_to_data_set.md)
    then stops with “Supplied 4 items to be assigned to 3 items of
    column ‘granularity_geo’”.

- Brought the prose in `R/`, `vignettes/`, `README.md`, `index.md` and
  `NEWS.md` to the house standard: ASD-STE100 (Simplified Technical
  English), adapted. Split the long sentences, removed the em dashes
  from `index.md`, and capitalised the RFC-2119 keywords in the coding
  rules of
  [`vignette("csdata")`](https://niphr.github.io/csdata/articles/csdata.md).

  - Sentences over 25 words, measured per authored unit, before and
    after:
    - `R/`: 8 to 0.
    - `vignettes/csdata.Rmd`: 1 to 0.
    - `README.md`: 1 to 0.
    - `NEWS.md`: 10 to 0.
    - `index.md`, `vignettes/locations_norway.Rmd` and
      `vignettes/population_norway.Rmd`: 0 before and after.
  - Regenerated `man/` from the edited roxygen. `NAMESPACE` is
    unchanged, and all 13 help pages remain.

- No code, data, documented function behaviour or documented number
  changed in this version.

## Version 2026.8.4

- Documentation overhaul. Each of the four documentation homes now owns
  its own material:
  - `README.md` is the GitHub landing page: what the package is,
    install, one quick start, and a which-function-do-I-want table.
  - `index.md` is the pkgdown home body.
  - The vignettes carry the worked detail.
  - The roxygen reference carries the per-function contract.
- Grew `README.md` from 49 to 280 words and added the routing table. No
  passage is shared with a vignette.
- Added `@seealso` to all 13 exported objects. A vignette code chunk
  covers two of them:
  [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
  in all three articles, and
  [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
  in `population_norway`. The other 11 say plainly that no vignette
  covers them.
- Added `@family` to four groups that share a real call-site contract:
  location code converters, data set column adders, population data, and
  unicode character lists. `nor_locations_*()` was deliberately left
  ungrouped, because the three functions share only a name prefix and
  return disjoint columns.
- The
  [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
  and
  [`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md)
  examples run again. Version 2026.7.2 wrapped them in `\dontrun{}`,
  after a CRAN pretest NOTE on the CPU-to-elapsed ratio. Both now pin
  `data.table::setDTthreads(1)` for the duration of the example, and
  restore the previous setting.
- Corrected documentation that did not match the code:
  - [`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md)
    and
    [`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md)
    no longer claim to accept a plain `data.frame`. A `data.frame`
    reaches the default method. There the first warns and returns one
    value for the whole frame. The second returns one value per column
    rather than per row.
  - [`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md)
    and
    [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md)
    now state that they return `"nor"` without reading the input. Any
    string therefore yields `"nor"`, and
    [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md)
    succeeds on a table with no `location_code` column.
  - [`set_config()`](https://niphr.github.io/csdata/reference/set_config.md)
    returns the assigned value invisibly, not `NULL`, whenever
    `border_nor` is supplied.
  - [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
    lists all 20 values of `granularity_geo`, not 12. It also describes
    `location_name_short` as it is. That column is set on 38 of 616
    rows. It carries three letters for counties, and it is absent for
    the Bergen, Stavanger and Trondheim city districts.
  - [`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md)
    lists all 16 values of `granularity_geo`, not 14.
  - [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
    explains that `sex` is `"total"` because the function filters to the
    sex total, not because the dataset lacks the split.
  - [`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md)
    separates the two no-breakdown cases. Svalbard and Jan Mayen keep a
    real `total`. The unknown-location codes are `NA` on all three sex
    rows.
- Documented that
  [`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md)
  returns zero rows for every `from`/`to` combination involving
  `"baregion"`, because the bundled hierarchy table carries no BA-region
  codes. The example that demonstrated this by silently printing an
  empty table was replaced.
- Build-ignore `pkgdown/` and `Rplots.pdf`, which
  [`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
  and plotting scripts leave behind.

## Version 2026.7.27

- Fixed
  [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md).
  It assigned the ISO 3166-1 alpha-3 country code to the
  `granularity_geo` column instead of `iso3`. It therefore never created
  an `iso3` column, and it silently overwrote any existing
  `granularity_geo`. It now assigns to `iso3` and leaves
  `granularity_geo` untouched.
- Corrected the documented return value of
  [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md),
  which described the bug as intended behaviour.
- Added a test suite pinning
  [`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md),
  [`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md),
  [`add_granularity_geo_to_data_set()`](https://niphr.github.io/csdata/reference/add_granularity_geo_to_data_set.md)
  and
  [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md).

## Version 2026.7.2

CRAN release: 2026-07-02

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
- Note: at redistricted sub-national levels the pipeline rounds each sex
  independently. `male + female` may therefore differ from `total` by up
  to 3 people, in about 0.6% of historical cells. The national level is
  exact.

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
