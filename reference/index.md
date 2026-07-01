# Package index

## Location data

Reference datasets for Norwegian geographic locations: codes, names,
hierarchies, and redistricting weights.

- [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
  : Location codes and names for Norwegian geographic units
- [`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md)
  : Location hierarchies in Norway
- [`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md)
  : Redistricting weights for Norwegian geographic units

## Population data

Norwegian population counts aggregated by municipality and custom age
categories, optionally split by sex.

- [`nor_population_by_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_age_cats.md)
  : Norwegian population aggregated into custom age categories
- [`nor_population_by_sex_age_cats()`](https://niphr.github.io/csdata/reference/nor_population_by_sex_age_cats.md)
  : Population in Norway by sex and age categories

## Location-code utilities

Functions that derive or attach geographic metadata from location codes.

- [`location_code_to_granularity_geo()`](https://niphr.github.io/csdata/reference/location_code_to_granularity_geo.md)
  : Convert location codes to granularity_geo
- [`location_code_to_iso3()`](https://niphr.github.io/csdata/reference/location_code_to_iso3.md)
  : Convert location codes to ISO 3166-1 alpha-3 country codes
- [`add_granularity_geo_to_data_set()`](https://niphr.github.io/csdata/reference/add_granularity_geo_to_data_set.md)
  : Add a granularity_geo column to a data set
- [`add_iso3_to_data_set()`](https://niphr.github.io/csdata/reference/add_iso3_to_data_set.md)
  : Add an iso3 column to a data set

## Package configuration

Read and update package-wide settings such as the active border year.

- [`config`](https://niphr.github.io/csdata/reference/config.md) :
  Package configuration environment
- [`set_config()`](https://niphr.github.io/csdata/reference/set_config.md)
  : Set package configuration options

## Character utilities

Named lists of Norwegian and Swedish special characters for use in
scripts and labels.

- [`nb`](https://niphr.github.io/csdata/reference/nb.md) : Norwegian
  characters in unicode
- [`se`](https://niphr.github.io/csdata/reference/se.md) : Swedish
  characters in unicode
