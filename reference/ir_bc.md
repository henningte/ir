# Performs baseline correction on infrared spectra

`ir_bc` performs baseline correction for infrared spectra. Baseline
correction is either performed by using a polynomial with user defined
degree fitted to each spectrum (see
[`ChemoSpec::baselineSpectra()`](https://bryanhanson.github.io/ChemoSpec/reference/baselineSpectra.html)),
or by using a rubberband function that is fitted to each spectrum (see
[`hyperSpec::spc.rubberband()`](https://r-hyperspec.github.io/hyperSpec/reference/spc-rubberband.html)),
or using a Savitzky-Golay smoothed version of the input spectra (see
[`ir_bc_sg()`](https://henningte.github.io/ir/reference/ir_bc_sg.md)).

## Usage

``` r
ir_bc(x, method = "rubberband", return_bl = FALSE, ...)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- method:

  A character value indicating which method should be used for baseline
  correction. If `method = "polynomial"`, a polynomial is used for
  baseline correction. If `method = "rubberband"`, a rubberband function
  is used for baseline correction. If `method = "sg"`, a Savitzky-Golay
  smoothed version of the input spectra is used for baseline correction.

- return_bl:

  A logical value indicating if for each spectrum the baseline should be
  returned instead of the corrected intensity values
  (`return_bl = TRUE`) or not (`return_bl = FALSE`).

- ...:

  Further arguments passed to
  [`ir_bc_polynomial()`](https://henningte.github.io/ir/reference/ir_bc_polynomial.md),
  [`ir_bc_rubberband()`](https://henningte.github.io/ir/reference/ir_bc_rubberband.md)
  or
  [`ir_bc_sg()`](https://henningte.github.io/ir/reference/ir_bc_sg.md).

## Value

An object of class `ir` with the baseline corrected spectra, or if
`return_bl = TRUE`, the baselines instead of the spectra in column
`spectra`.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# rubberband baseline correction
x1 <-
   ir::ir_sample_data |>
   dplyr::slice(1:10) |>
   ir::ir_bc(method = "rubberband")

# polynomial baseline correction
if(!requireNamespace("ChemoSpec", quietly = TRUE)) {
  x2 <-
    ir::ir_sample_data |>
    dplyr::slice(1:10) |>
    ir::ir_bc(method = "polynomial", degree = 2)
}

# Savitzky-Golay baseline correction
if(!requireNamespace("signal", quietly = TRUE)) {
  x3 <-
     ir::ir_sample_data |>
     dplyr::slice(1:10) |>
     ir::ir_bc(method = "sg", p = 3, n = 199, ts = 1, m = 0)
}

# return the baseline instead of the baseline corrected spectra
x1_bl <-
   ir::ir_sample_data |>
   dplyr::slice(1:10) |>
   ir::ir_bc(method = "rubberband", return_bl = TRUE)
```
