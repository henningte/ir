# Interpolates selected regions in infrared spectra in an `ir` object

`ir_interpolate_region` linearly interpolates a user-defined region in
infrared spectra.

## Usage

``` r
ir_interpolate_region(x, range)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- range:

  A `data.frame` with a row for each region to interpolate linearly and
  two columns:

  start

  :   A numeric vector with start values for regions to interpolate
      linearly (x axis values).

  end

  :   A numeric vector with end values for regions to interpolate
      linearly (x axis values).

  For each row in `range`, the values in `range$start` have to be
  smaller than the values in `range$end`.

## Value

`x` with the defined wavenumber region(s) interpolated linearly.

## Examples

``` r
# interpolation range
range <- data.frame(start = 1000, end = 1500)

# do the interpolation
x <-
   ir::ir_sample_data |>
   ir::ir_interpolate_region(range = range)
```
