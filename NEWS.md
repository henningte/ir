# ir (development version)

## Bug fixes

* In `ir_import_spc()`, the returned `ir` object was not a `tbl_df`, but a data frame. This is now corrected.
* In `unnest.ir()`, the `ir` class was not restored and therefore the result was always unclassed.
* In `ir_average()`, the returned `ir` object was still a `grouped_df`. This is now corrected (by adding the `.groups` argument for `dplyr::summarize()` which is internally used by `ir_average()`).
* In `ir_import_spc()`, the returned `ir` object was a subclass of `data.frame` and not `tbl_df`.

## New functions
* `ir_export_prepare()` to support exporting `ir` objects to `csv`.

## Improvements

* Making `ir_flatten()` faster by improving spectra combining.
* Making `ir_bin()` faster by improving spectra summarizing per bin. Allowing to specify how wavenumber values should be defined after binning. 

# ir 0.2.1

* Circumventing a bug in `ir_import_spc()` by allowing to not export spectra metadata. This bug is caused by `hyperSpec::read.spc()` and will be fixed in the upcoming weeks. See https://github.com/r-hyperspec/hyperSpec/issues/80


# ir 0.2.0

* First CRAN release (#1).
* Added a `NEWS.md` file to track changes to the package.
* Added tidyverse methods for `ir` objects.
* `ir` objects no longer require columns `measurement_id` and `sample_id`, but only the column `spectra`.
* Added subsetting and binding methods for `ir` objects.
* Added arithmetic operators for `ir` objects.
