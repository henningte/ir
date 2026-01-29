# Package index

## Overview

Pacakge overview

- [`ir`](https://henningte.github.io/ir/reference/ir-package.md)
  [`ir-package`](https://henningte.github.io/ir/reference/ir-package.md)
  : ir: Functions to Handle and Preprocess Infrared Spectra

## ir-class

Create `ir`-objects, subset `ir` objects

- [`ir_new_ir()`](https://henningte.github.io/ir/reference/ir_new_ir.md)
  :

  Creates an object of class `ir`

- [`ir_as_ir()`](https://henningte.github.io/ir/reference/ir_as_ir.md) :

  Converts an object to class `ir`

- [`ir_drop_spectra()`](https://henningte.github.io/ir/reference/ir_drop_spectra.md)
  :

  Drops the column `spectra` from an object is of class `ir`

- [`ir_get_intensity()`](https://henningte.github.io/ir/reference/ir_get_intensity.md)
  :

  Extracts intensities from spectra in an `ir` object for specific
  spectral channels

- [`ir_get_wavenumberindex()`](https://henningte.github.io/ir/reference/ir_get_wavenumberindex.md)
  : Gets the index of a defined wavenumber value for a spectrum

- [`ir_get_spectrum()`](https://henningte.github.io/ir/reference/ir_get_spectrum.md)
  :

  Extracts selected spectra from an object of class `ir`

- [`ir_remove_missing()`](https://henningte.github.io/ir/reference/ir_remove_missing.md)
  :

  Removes empty data values in an object of class `ir`

- [`ir_flatten()`](https://henningte.github.io/ir/reference/ir_flatten.md)
  :

  Converts objects of class `ir` to objects of class `ir_flat`

- [`rep(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/rep.ir.md)
  : Replicate ir objects

- [`rbind(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/bind.md)
  [`cbind(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/bind.md)
  :

  Bind rows of `ir` objects

- [`` `[`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  [`` `$`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  [`` `[[`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  [`` `$<-`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  [`` `[<-`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  [`` `[[<-`( ``*`<ir>`*`)`](https://henningte.github.io/ir/reference/subsetting.md)
  :

  Subsetting `ir` objects

## Plotting spectra

Plotting spectra

- [`plot(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/plot.ir.md)
  :

  Plots an object of class `ir`

## Import

Import of spectra

- [`ir_import_csv()`](https://henningte.github.io/ir/reference/ir_import_csv.md)
  : Imports infrared spectra from various files
- [`ir_import_spc()`](https://henningte.github.io/ir/reference/ir_import_spc.md)
  : Imports infrared spectra from Thermo Galactic's files

## Export

Export of spectra

- [`ir_export_prepare()`](https://henningte.github.io/ir/reference/ir_export_prepare.md)
  :

  Prepares `ir` objects for export to `csv`

## Spectral processing

Spectral preprocessing

- [`ir_average()`](https://henningte.github.io/ir/reference/ir_average.md)
  : Averages infrared spectra within groups

- [`ir_bc()`](https://henningte.github.io/ir/reference/ir_bc.md) :
  Performs baseline correction on infrared spectra

- [`ir_bin()`](https://henningte.github.io/ir/reference/ir_bin.md) :
  Bins infrared spectra

- [`ir_clip()`](https://henningte.github.io/ir/reference/ir_clip.md) :
  Clips infrared spectra to new wavenumber ranges

- [`ir_interpolate()`](https://henningte.github.io/ir/reference/ir_interpolate.md)
  :

  Interpolates intensity values of infrared spectra in an `ir` object
  for new wavenumber values

- [`ir_interpolate_region()`](https://henningte.github.io/ir/reference/ir_interpolate_region.md)
  :

  Interpolates selected regions in infrared spectra in an `ir` object

- [`ir_normalize()`](https://henningte.github.io/ir/reference/ir_normalize.md)
  [`ir_normalise()`](https://henningte.github.io/ir/reference/ir_normalize.md)
  :

  Normalizes infrared spectra in an `ir` object

- [`ir_smooth()`](https://henningte.github.io/ir/reference/ir_smooth.md)
  :

  Smooths infrared spectra in an `ir` object

- [`ir_correct_atmosphere()`](https://henningte.github.io/ir/reference/ir_correct_atmosphere.md)
  : Corrects artifacts in a spectrum based on reference spectra of the
  artifact compound

- [`ir_scale()`](https://henningte.github.io/ir/reference/ir_scale.md) :

  Scales spectra in an `ir` object

- [`ir_to_transmittance()`](https://henningte.github.io/ir/reference/ir_to_transmittance.md)
  [`ir_to_absorbance()`](https://henningte.github.io/ir/reference/ir_to_transmittance.md)
  : Converts absorbance spectra to transmittance spectra or vice versa

## Spectral analysis

Spectral analysis

- [`Ops(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/Ops.ir.md)
  :

  Arithmetic operations for `ir` objects

- [`ir_add()`](https://henningte.github.io/ir/reference/ir_add.md) : Add
  infrared spectra

- [`ir_subtract()`](https://henningte.github.io/ir/reference/ir_subtract.md)
  : Subtract infrared spectra

- [`ir_multiply()`](https://henningte.github.io/ir/reference/ir_multiply.md)
  : Multiply infrared spectra or multiply infrared spectra with a
  numeric value

- [`ir_divide()`](https://henningte.github.io/ir/reference/ir_divide.md)
  : Divide infrared spectra or divide infrared spectra by a numeric
  value

- [`ir_variance_region()`](https://henningte.github.io/ir/reference/ir_variance_region.md)
  :

  Computes the variance of a spectrum in an `ir` object in a given
  region

- [`range(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/range.md)
  [`min(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/range.md)
  [`max(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/range.md)
  [`median(`*`<ir>`*`)`](https://henningte.github.io/ir/reference/range.md)
  : Get the minima/maxima/range/median of x axis values or intensity
  values of infrared spectra

- [`ir_sample_prospectr()`](https://henningte.github.io/ir/reference/ir_sample_prospectr.md)
  : Wrapper to sampling functions from the 'prospectr' package

## tidyverse methods

tidyverse methods for `ir` objects

- [`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md)
  :

  Arrange rows in `ir` objects by column values

- [`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md)
  :

  Subset distinct/unique rows in `ir` objects

- [`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md)
  :

  Extract a character column in an `ir` object into multiple columns
  using regular expression groups

- [`semi_join.ir()`](https://henningte.github.io/ir/reference/filter-joins.md)
  [`anti_join.ir()`](https://henningte.github.io/ir/reference/filter-joins.md)
  :

  Filtering joins for an `ir` object

- [`filter.ir()`](https://henningte.github.io/ir/reference/filter.ir.md)
  :

  Subset rows in `ir` objects using column values

- [`group_by.ir()`](https://henningte.github.io/ir/reference/group_by.md)
  [`ungroup.ir()`](https://henningte.github.io/ir/reference/group_by.md)
  :

  Group rows in `ir` objects by one or more variables

- [`inner_join.ir()`](https://henningte.github.io/ir/reference/mutate-joins.md)
  [`left_join.ir()`](https://henningte.github.io/ir/reference/mutate-joins.md)
  [`right_join.ir()`](https://henningte.github.io/ir/reference/mutate-joins.md)
  [`full_join.ir()`](https://henningte.github.io/ir/reference/mutate-joins.md)
  :

  Mutating joins for an `ir` object

- [`mutate.ir()`](https://henningte.github.io/ir/reference/mutate.md)
  [`transmute.ir()`](https://henningte.github.io/ir/reference/mutate.md)
  :

  Mutate an `ir` object by adding new or replacing existing columns

- [`nest.ir()`](https://henningte.github.io/ir/reference/nest.md)
  [`unnest.ir()`](https://henningte.github.io/ir/reference/nest.md) :

  Nest and un-nest an `ir` object

- [`pivot_longer.ir()`](https://henningte.github.io/ir/reference/pivot_longer.ir.md)
  :

  Pivot an `ir` object from wide to long

- [`pivot_wider.ir()`](https://henningte.github.io/ir/reference/pivot_wider.ir.md)
  :

  Pivot an `ir` object from wide to long

- [`rename.ir()`](https://henningte.github.io/ir/reference/rename.md)
  [`rename_with.ir()`](https://henningte.github.io/ir/reference/rename.md)
  :

  Rename columns in `ir` objects

- [`rowwise.ir()`](https://henningte.github.io/ir/reference/rowwise.ir.md)
  :

  Group input `ir` objects by rows

- [`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md)
  :

  Subset columns in `ir` objects using column names and types

- [`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md)
  :

  Separate a character column in an `ir` object into multiple columns
  with a regular expression or numeric locations

- [`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md)
  :

  Separate a collapsed column in an `ir` object into multiple rows

- [`slice.ir()`](https://henningte.github.io/ir/reference/slice.md)
  [`slice_sample.ir()`](https://henningte.github.io/ir/reference/slice.md)
  :

  Subset rows in `ir` objects using their positions

- [`summarize.ir()`](https://henningte.github.io/ir/reference/summarize.md)
  [`summarise.ir()`](https://henningte.github.io/ir/reference/summarize.md)
  :

  Summarize each group in a `ir` object to fewer rows

- [`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md) :

  Unite multiple columns in an `ir` object into one by pasting strings
  together

## Sample data

Sample dataset

- [`ir_sample_data`](https://henningte.github.io/ir/reference/ir_sample_data.md)
  :

  Sample object of class `ir`
