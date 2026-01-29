# Corrects artifacts in a spectrum based on reference spectra of the artifact compound

`ir_correct_atmosphere` takes two objects of class `ir` with the same
number of spectra in each and corrects the spectra of the first object
with the spectra of the second object according to the procedure
presented by (Perez-Guaita et al. 2013) .

## Usage

``` r
ir_correct_atmosphere(
  x,
  ref,
  wn1,
  wn2,
  return_contribution = FALSE,
  do_interpolate = FALSE,
  start = NULL,
  dw = 1,
  warn = TRUE,
  return_ir_flat = FALSE
)
```

## Source

Perez-Guaita D, Kuligowski J, Quintás G, Garrigues S, de la Guardia M
(2013). “Atmospheric Compensation in Fourier Transform Infrared (FT-IR)
Spectra of Clinical Samples.” *Applied Spectroscopy*, **67**(11),
1339–1342. ISSN 0003-7028, 1943-3530,
[doi:10.1366/13-07159](https://doi.org/10.1366/13-07159) .

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md)
  containing the spectra to correct (with intensities representing
  absorbances).

- ref:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md)
  containing the reference spectra to use for correction (with
  intensities representing absorbances). `ref` must have the same number
  of rows as `x`, the contained spectra must cover the wavenumber range
  of all spectra in `x`, and if `do_interpolate = FALSE`, all spectra
  must have identical wavenumber values.

- wn1:

  A numeric value representing the first wavenumber value to use as
  reference point (Perez-Guaita et al. 2013) . Examples used by
  Perez-Guaita et al. (2013) are:

  H\\\_2\\O

  :   3902 cm\\^{-1}\\.

  CO\\\_2\\

  :   2361 cm\\^{-1}\\.

- wn2:

  A numeric value representing the second wavenumber value to use as
  reference point (Perez-Guaita et al. 2013) . Examples used by
  Perez-Guaita et al. (2013) are:

  H\\\_2\\O

  :   3912 cm\\^{-1}\\.

  CO\\\_2\\

  :   2349 cm\\^{-1}\\.

- return_contribution:

  A logical value indicating whether in addition to the corrected
  spectra, the computed relative contribution of `ref` to each spectrum
  in `x` should be added to the returned object as new column
  `contribution` (`TRUE`) or not (`FALSE`).

- do_interpolate:

  A logical value indicating if `x` and `ref` should be interpolated
  prior correction (`TRUE`) or not (`FALSE`).

- start:

  See
  [`ir_interpolate()`](https://henningte.github.io/ir/reference/ir_interpolate.md).

- dw:

  See
  [`ir_interpolate()`](https://henningte.github.io/ir/reference/ir_interpolate.md).

- warn:

  A logical value indicating whether warnings about mismatching
  wavenumber values should be displayed (`TRUE`) or not (`FALSE`). If
  set to `TRUE` and `wn1` or `wn2` do not exactly match the wavenumber
  values in `x` and `ref`, a warning will be printed to inform about the
  wavenumber difference between the selected and targeted wavenumber
  value.

- return_ir_flat:

  Logical value. If `TRUE`, the spectra are returned as
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md)
  object.

## Value

`x` corrected with the reference spectra in `ref`.

## Examples

``` r
x1 <-
  ir_correct_atmosphere(
    ir_sample_data[1:5, ], ir_sample_data[1:5, ], wn1 = 2361, wn2 = 2349
  )

x2 <-
  ir_correct_atmosphere(
    ir_sample_data[1:5, ], ir_sample_data[1:5, ], wn1 = 2361, wn2 = 2349,
    return_contribution = TRUE
  )

x2$contribution
#> 1 2 3 4 5 
#> 1 1 1 1 1 
#> attr(,"class")
#> [1] "numeric"
```
