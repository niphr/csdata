# Resubmission

This is a resubmission of 2026.7.1, which was archived by the incoming
pre-test with one NOTE on the Debian flavor: the `nor_population_by_age_cats`
example exceeded the 5s example time limit (and the user/elapsed ratio
threshold, due to data.table multi-threading). That example aggregates the
full population dataset, so it is now wrapped in `\dontrun{}` with an
explanatory comment. No other changes.

# Submission

This is a feature release that also removes a deprecated option.

## Summary of changes

* Population data now carries a sex dimension, and a new exported function
  `nor_population_by_sex_age_cats()` returns population by age category split
  by sex.
* Breaking change: support for the 2020 geographic border year has been
  removed. All data-returning functions now accept only `border = 2024`
  (`border = 2020` and `set_config(border_nor = 2020)` now error). See NEWS.md.

## Test environments

* local: Ubuntu 22.04, R 4.6.0
* R-hub: Ubuntu Linux (R-devel), Windows Server (R-devel), macOS (R-devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

The R-hub runs on all three platforms reported a single NOTE for a
non-standard top-level file ('index.md', which is source for the pkgdown
site). That file has been added to .Rbuildignore and no longer ships in the
package, clearing the NOTE.

## Downstream dependencies

csdata has three reverse dependencies on CRAN: csalert, csmaps, and cstidy.
This release removes the 2020 border option. None of the three call the
removed code path: they pass no `border = 2020` argument to csdata's
functions and do not use the former internal b2020 datasets, so they are
expected to be unaffected.
