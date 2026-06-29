# csdata

`csdata` provides structural data for Norway.

Please see <https://niphr.github.io/csdata/reference/index.html> for a
quick overview of all available datasets and functions.

## Location

Valid locations (and location types) are available in
[`csdata::nor_locations_names()`](https://niphr.github.io/csdata/reference/nor_locations_names.md).
Uncommon/internal use is demarcated by a line through the text.

[TABLE]

## Ages

Ages should be coded as characters and should always contain 3 digits.
If it is an age range, the two ages are joined by an underscore
(e.g. `005_010`).

Use `085p` instead of `>=085` or `85+`, as this will allow for an easy
conversion from long to wide formatted data.

| Valid ages in the csverse format |           |                                  |
|----------------------------------|-----------|----------------------------------|
| Value                            | class     | Definition                       |
| "000"                            | character | One year age group (0 year olds) |
| "079"                            | character | One year age group(79 year olds) |
| "000_004"                        | character | Age span of 0-4 year olds        |
| "065p"                           | character | Age span of \>=65 year olds      |
| "missing"                        | character | Missing/unknown                  |
| "total"                          | character | Everyone                         |

This format will help your data be easily sorted, kept in the right
order, and generate valid variable names if converted to wide-format.

Missing ages should be coded as “missing”.

## Sex

Sex should be coded as characters.

| Valid sexes in the csverse format |           |                 |
|-----------------------------------|-----------|-----------------|
| Value                             | class     | Definition      |
| "male"                            | character | Male            |
| "female"                          | character | Female          |
| "missing"                         | character | Missing/unknown |
| "total"                           | character | Everyone        |

Missing sexes should be coded as “missing”.
