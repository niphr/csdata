# Version 2026.8.4

- Documentation overhaul. Each of the four documentation homes now owns its own material: `README.md` is the GitHub landing page (what the package is, install, one quick start, a which-function-do-I-want table), `index.md` is the pkgdown home body, the vignettes carry the worked detail, and the roxygen reference carries the per-function contract.
- Grew `README.md` from 49 to 280 words and added the routing table. No passage is shared with a vignette.
- Added `@seealso` to all 13 exported objects. Two of them are covered by a vignette code chunk (`nor_locations_names()` in all three articles, `nor_population_by_age_cats()` in `population_norway`); the other 11 say plainly that no vignette covers them.
- Added `@family` to four groups that share a real call-site contract: location code converters, data set column adders, population data, and unicode character lists. `nor_locations_*()` was deliberately left ungrouped, because the three functions share only a name prefix and return disjoint columns.
- The `nor_population_by_age_cats()` and `nor_population_by_sex_age_cats()` examples run again. They were wrapped in `\dontrun{}` in 2026.7.2 after a CRAN pretest NOTE on the CPU-to-elapsed ratio; both now pin `data.table::setDTthreads(1)` for the duration of the example and restore the previous setting.
- Corrected documentation that did not match the code:
  - `location_code_to_granularity_geo()` and `location_code_to_iso3()` no longer claim to accept a plain `data.frame`. A `data.frame` reaches the default method, where the first warns and returns one value for the whole frame and the second returns one value per column rather than per row.
  - `location_code_to_iso3()` and `add_iso3_to_data_set()` now state that `"nor"` is returned without reading the input, so any string yields `"nor"` and `add_iso3_to_data_set()` succeeds on a table with no `location_code` column.
  - `set_config()` returns the assigned value invisibly, not `NULL`, whenever `border_nor` is supplied.
  - `nor_locations_names()` lists all 20 values of `granularity_geo`, not 12, and describes `location_name_short` as it is: set on 38 of 616 rows, three letters for counties, and absent for the Bergen, Stavanger and Trondheim city districts.
  - `nor_locations_redistricting()` lists all 16 values of `granularity_geo`, not 14.
  - `nor_population_by_age_cats()` explains that `sex` is `"total"` because the function filters to the sex total, not because the dataset lacks the split.
  - `nor_population_by_sex_age_cats()` separates the two no-breakdown cases: Svalbard and Jan Mayen keep a real `total`, while the unknown-location codes are `NA` on all three sex rows.
- Documented that `nor_locations_hierarchy_from_to()` returns zero rows for every `from`/`to` combination involving `"baregion"`, because the bundled hierarchy table carries no BA-region codes. The example that demonstrated this by silently printing an empty table was replaced.
- Build-ignore `pkgdown/` and `Rplots.pdf`, which `pkgdown::build_site()` and plotting scripts leave behind.

# Version 2026.7.27

- Fixed `add_iso3_to_data_set()`. It assigned the ISO 3166-1 alpha-3 country code to the `granularity_geo` column instead of `iso3`. It therefore never created an `iso3` column, and it silently overwrote any existing `granularity_geo`. It now assigns to `iso3` and leaves `granularity_geo` untouched.
- Corrected the documented return value of `add_iso3_to_data_set()`, which described the bug as intended behaviour.
- Added a test suite pinning `location_code_to_granularity_geo()`, `location_code_to_iso3()`, `add_granularity_geo_to_data_set()` and `add_iso3_to_data_set()`.

# Version 2026.7.2

- **Breaking:** dropped support for the 2020 border year. All `_b2020` datasets are removed and every data-returning function now accepts only `border = 2024` (`set_config(border_nor = 2020)` and `border = 2020` now error). Use `border = 2024`.
- Population data now carries a sex dimension. `nor_population_by_age_b2024` includes real `male`/`female`/`total` splits (fetched from SSB tables 07459 and 10826 with `Kjonn = TRUE`). Locations where Statistics Norway provides no sex breakdown (Svalbard, Jan Mayen, unknown) carry `total` with `NA` male/female.
- New exported function `nor_population_by_sex_age_cats()` returns population by custom age categories split by sex, with `include_total_age` and `include_total_sex` toggles.
- `nor_population_by_age_cats()` is unchanged for callers: it now filters to `sex == "total"` internally, so its output remains identical (total only).
- Note: at redistricted sub-national levels each sex is rounded independently, so `male + female` may differ from `total` by up to 3 people in ~0.6% of historical cells (exact at the national level).

# Version 2026.3.30

- Population data pipeline now fetches directly from SSB PxWeb API (tables 07459 and 10826) instead of manually downloaded CSV files.
- Including 2025 and 2026 population data.
- Municipality population data now uses actual historical data per municipality code instead of crude split approximations for merged municipalities.
- National population data sourced from table 07459 (1986+), replacing the defunct dataset 59322.
- Svalbard population data now fetched from SSB table 07430 (up to 2026), replacing the static Excel file.
- Removed unused Nordic country population and location Excel files.

# Version 2024.9.26

- location_name_file_nb_ascii in csdata::nor_location_names is now in lowercase.

# Version 2024.4.26

- Fixing an error that wouldn't allow for include_total = TRUE in nor_population_by_age_cats
- CRAN submission.

# Version 2024.4.21

- Including 2024 population data.

# Version 2024.1.17

- Fixing an error in merging for 2023->2024.
- Including population for georegion.

# Version 2023.12.26

- Improving names for georegion for border=2024.

# Version 2023.11.29

- Fixing error in redistricting of counties for border=2024.

# Version 2023.8.21

- Fixing error in the name Trøndelag-Trööndelage.

# Version 2023.7.31

- Including Sami names for counties.

# Version 2023.6.19

- Inclusion of location_name_short in nor_locations_names for border=2020.
- CRAN submission.

# Version 2023.5.26

- Inclusion of location_name_short in nor_locations_names for border=2024.

# Version 2023.5.22

- Including redistricting data for [2024](https://www.regjeringen.no/no/tema/kommuner-og-regioner/kommunestruktur/nye-kommune-og-fylkesnummer-fra-1.-januar-2024/id2924701/?expand=factbox2924711)
- Setting default for border_nor to 2024.
- CRAN submission.

# Version 2023.4.21

- Include population data for 2023.
