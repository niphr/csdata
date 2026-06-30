# csdata

`csdata` provides structural reference data for Norway: location
hierarchies, population counts, and the conventions used across the
csverse format.

See <https://niphr.github.io/csdata/reference/index.html> for an
overview of all available datasets and functions.

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
