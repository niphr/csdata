# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What This Package Is

`csdata` is a CRAN-published R package providing structural reference
data for Norway: population by age/municipality, location hierarchies
(ward/municipality/county/region), location names, and redistricting
mappings from 2006 to 2024. All data is versioned by geographic border
year (2020 or 2024), controlled via `csdata::config$border_nor`.

## Build and Check Commands

``` bash
# Full R CMD check (same as CI)
R CMD build . && R CMD check csdata_*.tar.gz --no-manual --as-cran

# Install locally for testing
Rscript -e 'devtools::install()'

# Generate documentation from roxygen2 comments
Rscript -e 'devtools::document()'

# Run package check via devtools
Rscript -e 'devtools::check()'

# Build pkgdown site
Rscript -e 'pkgdown::build_site()'
```

There are no unit tests (`tests/` directory does not exist). The package
relies on `R CMD check` and CI for validation.

## CI

GitHub Actions workflow (`.github/workflows/check-and-pkgdown.yml`) runs
on push to `main`/`develop` and PRs to `main`: 1. `R-CMD-check` with
`--no-manual --as-cran` 2. `pkgdown` site build and deploy to GitHub
Pages (on push to `main` only)

After pushing, run `gh run watch` to monitor CI.

## Architecture

### Data flow: raw files -\> internal datasets -\> exported functions

1.  **Raw source files** in `data-raw/files/` — Excel/CSV files with
    population data from SSB, location spreadsheets, BA-region mappings
2.  **Data-raw scripts** (`data-raw/01_*.R` through `data-raw/04_*.R`) —
    process raw files into internal datasets, run in numeric order
3.  **Internal datasets** stored in `R/sysdata.rda` — named by border
    year (e.g., `nor_locations_hierarchy_b2020`,
    `nor_population_by_age_b2024`)
4.  **Exported accessor functions** in `R/` — each function takes a
    `border` parameter and retrieves the corresponding internal dataset
    via `get0(..., envir = asNamespace("csdata"))`

### Border year system

All data-returning functions accept `border = csdata::config$border_nor`
(default 2024). Internal datasets are duplicated per border year
(`_b2020`, `_b2024`). The `set_config(border_nor=)` function changes the
package-wide default.

### Key exported functions

| Function                                                                                                             | File                              | Purpose                                                   |
|----------------------------------------------------------------------------------------------------------------------|-----------------------------------|-----------------------------------------------------------|
| [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)                           | `R/nor_locations_names.R`         | Location codes, names, display order, granularity         |
| [`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md)   | `R/nor_locations_hierarchy.R`     | Maps between geographic levels (e.g., municip -\> county) |
| [`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md)           | `R/nor_locations_redistricting.R` | Weightings to convert historical data to current borders  |
| [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)             | `R/nor_population_by_age_cats.R`  | Population aggregated by custom age categories            |
| [`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md) | `R/location_code_to.R`            | Extracts geographic granularity from location codes       |

### Location code conventions

Location codes follow the pattern `{granularity}{number}`: `nation_nor`,
`county_nor03`, `municip0301`, `wardoslo030101`, `baregion001`,
`missingmunicip9999`. The prefix determines the `granularity_geo` value.

### Data regeneration

To regenerate `R/sysdata.rda`, run the `data-raw/` scripts in order.
Intermediate results are cached in `data-raw/data-temp/` as `.rds`
files. Population data is fetched from SSB via the `PxWebApiData`
package.

## Code Conventions

- Package uses `data.table` throughout (not `dplyr`). All internal data
  manipulation uses `data.table` idioms: `:=`, `setnames`,
  `setcolorder`, `copy()`.
- S3 methods are used for `location_code_to_*` and `add_*_to_data_set`
  functions, dispatching on `data.table` vs `default`.
- Norwegian special characters are accessed via the `nb` helper list
  (`nb$aa`, `nb$oe`, `nb$ae` and uppercase variants).
- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`).
