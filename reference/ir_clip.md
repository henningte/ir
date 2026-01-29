# Clips infrared spectra to new wavenumber ranges

`ir_clip` clips infrared spectra to a new, specified, wavenumber range
or multiple new specified wavenumber ranges.

## Usage

``` r
ir_clip(x, range, return_ir_flat = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- range:

  A `data.frame` with two columns and a row for each wavenumber range to
  keep. The columns are:

  start

  :   A numeric vector with start values for wavenumber ranges.

  end

  :   A numeric vector with end values for wavenumber ranges.

  If `range` has more than one row, multiple ranges are clipped from `x`
  and merged together. Overlapping ranges are not allowed.

- return_ir_flat:

  Logical value. If `TRUE`, the spectra are returned as
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md)
  object.

## Value

An object of class `ir` (or `ir_flat`, if `return_ir_flat = TRUE`) where
spectra have been clipped.

## Examples

``` r
## clipping with one range

# define clipping range
range <-
  data.frame(start = 900, end = 1000)

# clip
x <-
   ir::ir_sample_data |>
   ir::ir_clip(range = range)

## clipping with mutliple ranges

# define clipping range
range <-
  data.frame(start = c(900, 1900), end = c(1000, 2200))

# clip
x <-
   ir::ir_sample_data |>
   ir::ir_clip(range = range)
```
