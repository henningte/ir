# Computes the variance of a spectrum in an `ir` object in a given region

`ir_variance_region` takes a spectrum `x` and, depending on the
arguments computes the following summary:

- if `subtract_smoothed = FALSE`:

  it computes the variance of the intensity values for each spectrum in
  `x`. If in addition `range` is not `NULL`, it computes the variance
  only for the region(s) represented by `range`.

- if `subtract_smoothed = TRUE`:

  it smoothes `x`, subtracts the smoothed `x` from the unsmoothed `x`
  and computes the variance of the difference intensity values. If in
  addition `range` is not `NULL`, it computes the variance only for the
  region(s) represented by `range`.

## Usage

``` r
ir_variance_region(
  x,
  subtract_smoothed = FALSE,
  do_normalize = FALSE,
  normalize_method,
  ...,
  range = NULL
)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md). These
  are the spectra for which to compute the variance.

- subtract_smoothed:

  A logical value. If `subtract_smoothed = TRUE`, `x` is copied, the
  copy smoothed using `ir_smooth` with `method = "sg"` and subtracted
  from `x` before the variance of the intensity values from `x` is
  computed. This allows e.g. to estimate the noise level in a specific
  region of spectra. If `subtract_smoothed = FALSE` (the default),
  nothing is subtracted from `x` before computing the variance of the
  intensity values.

- do_normalize:

  A logical value. If set to `TRUE`, the spectra in `x` are normalized
  after subtraction of a smoothed version, else no normalization is
  performed.

- normalize_method:

  See
  [`ir_normalize()`](https://henningte.github.io/ir/reference/ir_normalize.md).

- ...:

  Arguments passed to
  [`ir_smooth()`](https://henningte.github.io/ir/reference/ir_smooth.md)
  (except for `method` which is always set to `"sg"` if
  `subtract_smoothed` is `TRUE`). If `subtract_smoothed = FALSE`, these
  arguments will be ignored.

- range:

  See
  [`ir_clip()`](https://henningte.github.io/ir/reference/ir_clip.md).
  This is the range for which the variance of the intensity values will
  be computed.

## Value

`x` with two additional columns:

- variance:

  A numeric vector with the computed variances of the intensity values
  for the respective spectra.

- n_variance:

  An integer vector with the number of intensity values used during
  computing the variance.

## Examples

``` r
# Whole spectra variance
x1 <-
   ir::ir_sample_data |>
   ir::ir_variance_region(
      subtract_smoothed = FALSE,
      do_normalize = TRUE,
      normalize_method = "area",
      range = NULL
   )

# Spectra variance, but only from a specific region
range <- data.frame(start = 2700, end = 2800)

x2 <-
   ir::ir_sample_data |>
   ir::ir_normalize(method = "area") |>
   ir::ir_variance_region(
      subtract_smoothed = FALSE,
      do_normalize = TRUE,
      normalize_method = "area",
      range = range
   )

# Spectra variance after subtracting a smoothed version of the spectra and
# only from a specific region
x3 <-
   ir::ir_sample_data %>%
   ir::ir_variance_region(
      subtract_smoothed = TRUE,
      do_normalize = FALSE,
      range = range,
      p = 3, n = 31, ts = 1, m = 0
   )
```
