# Changelog

## ir 0.4.1

CRAN release: 2025-04-06

## ir 0.4.0

### Bug fixes

### New functions

- [`ir_scale()`](https://henningte.github.io/ir/reference/ir_scale.md):
  Function to scale intensity values in spectra, similar to
  [`base::scale()`](https://rdrr.io/r/base/scale.html). Intensity values
  are scaled for the same x axis values.
- [`ir_as_ir.ir_flat()`](https://henningte.github.io/ir/reference/ir_as_ir.md):
  Conversion of `ir_flat` objects to `ir` objects.
- [`ir_as_ir.hyperSpec()`](https://henningte.github.io/ir/reference/ir_as_ir.md):
  Conversion of
  [`hyperSpec::hyperSpec`](https://r-hyperspec.github.io/hyperSpec/reference/hyperSpec-package.html)
  objects to `ir` objects.
- [`ir_as_ir.Spectra()`](https://henningte.github.io/ir/reference/ir_as_ir.md):
  Conversion of
  [`ChemoSpec::Spectra`](https://bryanhanson.github.io/ChemoSpec/reference/Spectra.html)
  objects to `ir` objects.
- [`ir_identify_empty_spectra()`](https://henningte.github.io/ir/reference/ir_identify_empty_spectra.md):
  Identifies rows in an `ir` object with empty spectra (former internal
  function `ir_check_for_empty_spectra()`).
- [`ir_sample_prospectr()`](https://henningte.github.io/ir/reference/ir_sample_prospectr.md):
  Wrapper function to use sampling algorithms implemented in the
  ‘prospectr’ package directly with ‘ir’ objects.

### Improvements

- [`ir_correct_atmosphere()`](https://henningte.github.io/ir/reference/ir_correct_atmosphere.md)
  has a new parameter `return_contribution` which allows to return the
  contribution of `ref` to each spectrum in `x`.
- [`ir_normalize()`](https://henningte.github.io/ir/reference/ir_normalize.md)
  has three new methods to normalize spectra:
  1.  With `method = "area_absolute"`, all intensity values will be
      divided by the sum of the absolute intensity values at all
      wavenumber values of the spectrum.
  2.  With `method = "vector"`, all intensity values will be divided by
      the norm of the intensity vector (vector normalization).
  3.  With `method = "snv`“, a Standard Normal Variate correction will
      be performed.
- [`ir_bc_rubberband()`](https://henningte.github.io/ir/reference/ir_bc_rubberband.md)
  has an additional parameter to allow imputation of the first and last
  value in a spectra to avoid artifacts which may happen during baseline
  correction with the rubberband algorithm.
- All functions now fully support empty spectra.
- Several dependencies in `Imports` have been moved to `Suggests`.
- Efficiency of several functions has been improved.

## ir 0.3.0

### Bug fixes

- In
  [`ir_import_spc()`](https://henningte.github.io/ir/reference/ir_import_spc.md),
  the returned `ir` object was not a `tbl_df`, but a data frame. This is
  now corrected.
- In [`unnest.ir()`](https://henningte.github.io/ir/reference/nest.md),
  the `ir` class was not restored and therefore the result was always
  unclassed.
- In
  [`ir_average()`](https://henningte.github.io/ir/reference/ir_average.md),
  the returned `ir` object was still a `grouped_df`. This is now
  corrected (by adding the `.groups` argument for
  [`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
  which is internally used by
  [`ir_average()`](https://henningte.github.io/ir/reference/ir_average.md)).

### New functions

- [`ir_export_prepare()`](https://henningte.github.io/ir/reference/ir_export_prepare.md)
  to support exporting `ir` objects to `csv`.
- [`ir_correct_atmosphere()`](https://henningte.github.io/ir/reference/ir_correct_atmosphere.md)
  enables correcting for atmospheric artifacts (e.g. H$_{2}$O and
  CO$_{2}$ peaks) following Perez-Guaita et al. (2013) (DOI:
  10.1366/13-07159).

### Improvements

- Making
  [`ir_flatten()`](https://henningte.github.io/ir/reference/ir_flatten.md)
  faster by improving spectra combining.
- Making
  [`ir_bin()`](https://henningte.github.io/ir/reference/ir_bin.md)
  faster by improving spectra summarizing per bin. Allowing to specify
  how wavenumber values should be defined after binning.
- Arithmetic operations now also accept numeric vectors
  ([`ir_subtract()`](https://henningte.github.io/ir/reference/ir_subtract.md),
  [`ir_add()`](https://henningte.github.io/ir/reference/ir_add.md),
  [`ir_multiply()`](https://henningte.github.io/ir/reference/ir_multiply.md),
  [`ir_divide()`](https://henningte.github.io/ir/reference/ir_divide.md)).

## ir 0.2.1

CRAN release: 2022-05-02

- Circumventing a bug in
  [`ir_import_spc()`](https://henningte.github.io/ir/reference/ir_import_spc.md)
  by allowing to not export spectra metadata. This bug is caused by
  [`hyperSpec::read.spc()`](https://r-hyperspec.github.io/hyperSpec/reference/read-spc.html)
  and will be fixed in the upcoming weeks. See
  <https://github.com/r-hyperspec/hyperSpec/issues/80>

## ir 0.2.0

CRAN release: 2022-04-25

- First CRAN release ([\#1](https://github.com/henningte/ir/issues/1)).
- Added a `NEWS.md` file to track changes to the package.
- Added tidyverse methods for `ir` objects.
- `ir` objects no longer require columns `measurement_id` and
  `sample_id`, but only the column `spectra`.
- Added subsetting and binding methods for `ir` objects.
- Added arithmetic operators for `ir` objects.
