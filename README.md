# csdata <a href="https://niphr.github.io/csdata/"><img src="man/figures/logo.png" align="right" width="120" /></a>

[![CRAN status](https://www.r-pkg.org/badges/version/csdata)](https://cran.r-project.org/package=csdata)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/csdata)](https://cran.r-project.org/package=csdata)

## Overview

csdata ships preformatted structural data for Norway:

- geographic codes and display names;
- the hierarchy that links city districts to municipalities, counties and
  regions;
- yearly population counts by age and sex;
- the proportional weights that move historical figures onto current municipal
  borders.

The package bundles each dataset and returns it as a `data.table`. No call
reaches the network. Boundaries follow the 2024 administrative borders.
`border = 2024` is the only accepted value.

## Installation

```r
install.packages("csdata")

# development version
# remotes::install_github("niphr/csdata")
```

## Quick start

```r
library(csdata)

pop <- nor_population_by_age_cats(
  cats = list(children = 0:17, adults = 18:66, seniors = 67:105)
)
pop[location_code == "nation_nor" & calyear == 2024]
```

## Which function do I want?

| I need | Call |
|---|---|
| Codes, display names and presentation order for each geographic unit | `nor_locations_names()` |
| A mapping from one geographic level to another | `nor_locations_hierarchy_from_to()` |
| Weights that convert figures from older municipal borders to 2024 | `nor_locations_redistricting()` |
| Population counts aggregated into age bands I define | `nor_population_by_age_cats()` |
| Those counts split into male and female | `nor_population_by_sex_age_cats()` |
| The granularity label carried by a location code | `location_code_to_granularity_geo()` |
| That label written onto a data.table as a column | `add_granularity_geo_to_data_set()` |
| To read or set the package-wide default border year | `config`, `set_config()` |

## Documentation

The reference index and the three articles live at
<https://niphr.github.io/csdata/>. Start with the `csdata` article for the
coding conventions. Then read `locations_norway` for the geographic reference
table, and `population_norway` for the population series. Offline, run
`help(package = "csdata")`.
