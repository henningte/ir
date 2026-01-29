# Extracts selected spectra from an object of class `ir`

`ir_get_spectrum` extracts selected spectra from an object of class
`ir`.

## Usage

``` r
ir_get_spectrum(x, what)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- what:

  A numeric vector with each element representing a row in `x` for which
  to extract the spectrum.

## Value

An integer vector with the same length as `wavenumber` with the row
indices of `x` corresponding to the wavenumber values in `wavenumber`.

## Examples

``` r
x <-
   ir::ir_sample_data |>
   ir::ir_get_spectrum(what = c(5, 9))
```
