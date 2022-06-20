# ir (development version)

## Bug fixes

## New functions

## Improvements

* `ir_correct_atmosphere()` has a new parameter `return_contribution` which allows to return the contribution of `ref` to each spectrum in `x`.

# ir 0.3.0

## Bug fixes

* In `ir_import_spc()`, the returned `ir` object was not a `tbl_df`, but a data frame. This is now corrected.
* In `unnest.ir()`, the `ir` class was not restored and therefore the result was always unclassed.
* In `ir_average()`, the returned `ir` object was still a `grouped_df`. This is now corrected (by adding the `.groups` argument for `dplyr::summarize()` which is internally used by `ir_average()`).

## New functions

* `ir_export_prepare()` to support exporting `ir` objects to `csv`.
* `ir_correct_atmosphere()` enables correcting for atmospheric artifacts (e.g. H$_2$O and CO$_2$ peaks) following Perez-Guaita et al. (2013) (DOI: 10.1366/13-07159).  

## Improvements

* Making `ir_flatten()` faster by improving spectra combining.
* Making `ir_bin()` faster by improving spectra summarizing per bin. Allowing to specify how wavenumber values should be defined after binning. 
* Arithmetic operations now also accept numeric vectors (`ir_subtract()`, `ir_add()`, `ir_multiply()`, `ir_divide()`).

# ir 0.2.1

* Circumventing a bug in `ir_import_spc()` by allowing to not export spectra metadata. This bug is caused by `hyperSpec::read.spc()` and will be fixed in the upcoming weeks. See https://github.com/r-hyperspec/hyperSpec/issues/80


# ir 0.2.0

* First CRAN release (#1).
* Added a `NEWS.md` file to track changes to the package.
* Added tidyverse methods for `ir` objects.
* `ir` objects no longer require columns `measurement_id` and `sample_id`, but only the column `spectra`.
* Added subsetting and binding methods for `ir` objects.
* Added arithmetic operators for `ir` objects.
