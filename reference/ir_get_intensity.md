# Extracts intensities from spectra in an `ir` object for specific spectral channels

`ir_get_intensity` extracts intensity values of spectra for specific
user-defined spectral channels ("x axis values", e.g. wavenumber
values).

## Usage

``` r
ir_get_intensity(x, wavenumber, warn = TRUE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- wavenumber:

  A numeric vector with spectral channels ("x axis values", e.g.
  wavenumber values) for which to extract intensities.

- warn:

  logical value indicating if warnings should be displayed (`TRUE`) or
  not (`FALSE`).

## Value

`x` with an additional column `intensity`. `x$intensity` is a list
column with each element representing a `data.frame` with a row for each
element in `wavenumber` and two columns:

- x:

  The "x axis values" extracted with
  [`ir_get_wavenumberindex()`](https://henningte.github.io/ir/reference/ir_get_wavenumberindex.md)
  applied on `wavenumber` and the corresponding spectrum in `x`.

- y:

  The extracted intensity values

## Examples

``` r
x <-
   ir::ir_sample_data |>
   ir::ir_get_intensity(wavenumber = 1090)
```
