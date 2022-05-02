# ir 0.2.1

* Circumventing a bug in `ir_import_spc()` by allowing to not export spectra metadata. This bug is caused by `hyperSpec::read.spc()` and will be fixed in the upcoming weeks. See https://github.com/r-hyperspec/hyperSpec/issues/80

# ir 0.2.0

* First CRAN release (#1).
* Added a `NEWS.md` file to track changes to the package.
* Added tidyverse methods for `ir` objects.
* `ir` objects no longer require columns `measurement_id` and `sample_id`, but only the column `spectra`.
* Added subsetting and binding methods for `ir` objects.
* Added arithmetic operators for `ir` objects.
