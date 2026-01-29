# Smooths infrared spectra in an `ir` object

`ir_smooth` applies smoothing functions to infrared spectra. `ir_smooth`
either performs Savitzky-Golay smoothing, using on
[`signal::sgolayfilt()`](https://rdrr.io/pkg/signal/man/sgolayfilt.html),
or Fourier smoothing using
[`fda::smooth.basis()`](https://rdrr.io/pkg/fda/man/smooth.basis.html).
Savitzky-Golay smoothing can also be used to compute derivatives of
spectra.

## Usage

``` r
ir_smooth(
  x,
  method = "sg",
  p = 3,
  n = p + 3 - p%%2,
  ts = 1,
  m = 0,
  k = 111,
  ...
)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- method:

  A character value specifying which smoothing method to apply. If
  `method = "sg"`, a Savitzky-Golay filter will be applied on the
  spectra. The Savitzky-Golay smoothing will be performed using the
  function
  [`signal::sgolayfilt()`](https://rdrr.io/pkg/signal/man/sgolayfilt.html).
  If `method = "fourier"`, Fourier smoothing will be performed. Fourier
  transformation of the spectra is performed using the fast discrete
  Fourier transformation (FFT) as implemented in
  [`fda::smooth.basis()`](https://rdrr.io/pkg/fda/man/smooth.basis.html).
  A smoothing function can be defined by the argment `f`.

- p:

  An integer value representing the filter order (i.e. the degree of the
  polynom) of the Savitzky-Golay filter if `method = "sg"`.

- n:

  An odd integer value representing the length (i.e. the number of
  wavenumber values used to construct the polynom) of the Savitzky-Golay
  filter if `method = "sg"`.

- ts:

  time scaling factor. See
  [`signal::sgolayfilt()`](https://rdrr.io/pkg/signal/man/sgolayfilt.html).

- m:

  An integer value representing the mth derivative to compute. This
  option can be used to compute derivatives of spectra. See
  [`signal::sgolayfilt()`](https://rdrr.io/pkg/signal/man/sgolayfilt.html).

- k:

  A positive odd integer representing the number of Fourier basis
  functions to use as smoothed representation of the spectra if
  `method = "fourier"`.

- ...:

  additional arguments (ignored).

## Value

`x` with smoothed spectra.

## Details

When `x` contains spectra with different wavenumber values, the filters
are applied for each spectra only on existing wavenumber values. This
means that the filter window (if `method == "sg"`) will be different for
these different spectra.

## Examples

``` r
#' # Savitzky-Golay smoothing
if(! requireNamespace("signal", quietly = TRUE)) {
  x1 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 0)
}

# Fourier smoothing
if(! requireNamespace("fda", quietly = TRUE)) {
  x2 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "fourier", k = 21)
}

# computing derivative spectra with Savitzky-Golay smoothing (here: first
# derivative)
if(! requireNamespace("signal", quietly = TRUE)) {
  x3 <-
     ir::ir_sample_data[1:5, ] |>
     ir::ir_smooth(method = "sg", p = 3, n = 51, ts = 1, m = 1)
}
```
