# Interpolates intensity values of infrared spectra in an `ir` object for new wavenumber values

`ir_interpolate` interpolates intensity values for infrared spectra for
new wavenumber values.

## Usage

``` r
ir_interpolate(x, start = NULL, dw = 1, return_ir_flat = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- start:

  A numerical value indicating the start wavenumber value relative to
  which new wavenumber values will be interpolated. The value is not
  allowed to be \< `floor(firstvalue) - 2`, whereby `firstvalue` is the
  first wavenumber value within `x`. If `start = NULL`,
  `floor(firstvalue)` will be used as first wavenumber value.

- dw:

  A numerical value representing the desired wavenumber value difference
  between adjacent values.

- return_ir_flat:

  Logical value. If `TRUE`, the spectra are returned as
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md)
  object.

## Value

An object of class `ir` (or `ir_flat`, if `return_ir_flat = TRUE`),
containing the interpolated spectra. Any `NA` values resulting from
interpolation will be automatically dropped.

## Examples

``` r
x <-
   ir::ir_sample_data |>
   ir::ir_interpolate(start = NULL, dw = 1)
```
