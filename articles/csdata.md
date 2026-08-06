# csdata

`csdata` provides structural reference data for Norway: location
hierarchies, population counts, and the conventions used across the
csverse format.

See <https://niphr.github.io/csdata/reference/index.html> for an
overview of all available datasets and functions.

## What csdata is for

`csdata` holds the reference tables that Norwegian surveillance work
needs again and again: which places exist and how they nest inside each
other, how place boundaries were redistricted from 2006 to 2024, and how
many people live in each place by age and sex. Every table ships inside
the package, so nothing is downloaded when you run it. The point is that
each analysis stops carrying its own copy of the same lookups.

Thirteen objects are exported. These five do most of the work.

| Function                                                                                                   | Use it when you need                                                     |
|------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------|
| [`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)                 | The list of places, and which level each place sits at.                  |
| `nor_locations_hierarchy_from_to(from, to)`                                                                | To map one level onto another, such as every municipality to its county. |
| [`nor_locations_redistricting()`](https://niphr.github.io/csdata/reference/nor_locations_redistricting.md) | Weights to move data recorded under older borders onto the 2024 borders. |
| `nor_population_by_age_cats(cats)`                                                                         | Denominators: population in the age bands you choose.                    |
| `nor_population_by_sex_age_cats(cats)`                                                                     | The same denominators, split by sex.                                     |

All five take a `border` argument. `2024` is the only value they accept;
any other value stops with an error.

## Two current limitations

**BA-regions are missing from the hierarchy table.**
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
lists 159 BA-regions, and
[`nor_locations_hierarchy_from_to()`](https://niphr.github.io/csdata/reference/nor_locations_hierarchy_from_to.md)
accepts `"baregion"`. But the bundled hierarchy table carries no
BA-region codes at all, so every `from`/`to` pair naming `"baregion"`
returns an empty table instead of an error.

``` r
nrow(csdata::nor_locations_hierarchy_from_to(from = "municip", to = "baregion"))
#> [1] 0
nrow(csdata::nor_locations_names()[granularity_geo == "baregion"])
#> [1] 159
```

**One location code is used twice.** Two different laboratories share
the code `lab_nor084467`.

``` r
loc <- csdata::nor_locations_names()
loc[location_code == "lab_nor084467", .(location_code, location_name)]
#>    location_code              location_name
#>           <char>                     <char>
#> 1: lab_nor084467      VV-HF Drammen Sykehus
#> 2: lab_nor084467 Laboratoriet Bærum sykehus
```

This matters when you pass
[`nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md)
as `location_reference`, because the lookup is a join on
`location_code`. The duplicated code matches twice, so you get more
values back than you put in.

``` r
csdata::location_code_to_granularity_geo(
  c("nation_nor", "county_nor03", "lab_nor084467"),
  location_reference = loc
)
#> [1] "nation" "county" "lab"    "lab"
```

[`add_granularity_geo_to_data_set()`](https://niphr.github.io/csdata/reference/add_granularity_geo_to_data_set.md)
then fails, because it tries to write those four values into a three-row
table.

``` r
x <- data.table(location_code = c("nation_nor", "county_nor03", "lab_nor084467"))
csdata::add_granularity_geo_to_data_set(x, location_reference = loc)
#> Error in `[.data.table`:
#> ! Supplied 4 items to be assigned to 3 items of column 'granularity_geo'. If you wish to 'recycle' the RHS please use rep() to make this intent clear to readers of your code.
```

Leave `location_reference` unset to avoid both problems. The granularity
is then read from the code prefix, one value per input.

## Where csdata sits, and what to read next

`csdata` depends on no other cs package, so you can install it on its
own. `cstidy` imports it. `csmaps` uses the same `location_code` values
for the 2024 borders, so its map polygons join straight onto csdata
tables.

Two vignettes go deeper:

- [`vignette("locations_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/locations_norway.md)
  lists every location code with its name.
- [`vignette("population_norway", package = "csdata")`](https://niphr.github.io/csdata/articles/population_norway.md)
  shows population counts by location and calendar year.

The rest of this page gives the csverse coding rules for locations, ages
and sex.

## Location

Valid locations and location types are available in
[`csdata::nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md).
Entries with uncommon or internal use are shown with a strikethrough.

[TABLE]

## Ages

Ages must be coded as characters and must always contain three digits.
For age ranges, join the two ages with an underscore (e.g. `005_010`).

Use `085p` rather than `>=085` or `85+`; this keeps conversion from long
to wide format straightforward.

| Valid ages in the csverse format |           |                                  |
|----------------------------------|-----------|----------------------------------|
| Value                            | class     | Definition                       |
| "000"                            | character | One year age group (0 year olds) |
| "079"                            | character | One year age group(79 year olds) |
| "000_004"                        | character | Age span of 0-4 year olds        |
| "065p"                           | character | Age span of \>=65 year olds      |
| "missing"                        | character | Missing/unknown                  |
| "total"                          | character | Everyone                         |

This format keeps data sorted correctly and produces valid variable
names when pivoted to wide format.

Missing ages should be coded as `"missing"`.

## Sex

Sex must be coded as a character.

| Valid sexes in the csverse format |           |                 |
|-----------------------------------|-----------|-----------------|
| Value                             | class     | Definition      |
| "male"                            | character | Male            |
| "female"                          | character | Female          |
| "missing"                         | character | Missing/unknown |
| "total"                           | character | Everyone        |

Missing sex should be coded as `"missing"`.
