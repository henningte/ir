# Performs baseline correction on infrared spectra using a rubberband algorithm

`ir_bc_rubberband` performs baseline correction for infrared spectra
using a rubberband algorithm. `ir_bc_rubberband` is an extended wrapper
function for
[`hyperSpec::spc.rubberband()`](https://r-hyperspec.github.io/hyperSpec/reference/spc-rubberband.html).

## Usage

``` r
ir_bc_rubberband(x, do_impute = FALSE, return_bl = FALSE, ...)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- do_impute:

  A logical value indicating whether the in baseline the first and last
  values should be imputed with the second first and second last values,
  respectively (`TRUE`) or not (`FALSE`). This can be useful in case
  baseline correction without imputation causes artifacts which
  sometimes happens with this method.

- return_bl:

  A logical value indicating if for each spectrum the baseline should be
  returned instead of the corrected intensity values
  (`return_bl = TRUE`) or not (`return_bl = FALSE`).

- ...:

  Ignored.

## Value

An object of class `ir` with the baseline corrected spectra and, if
`returnbl = TRUE`, the baselines.

## See also

[`ir_bc()`](https://henningte.github.io/ir/reference/ir_bc.md)

## Examples

``` r
x1 <-
   ir::ir_sample_data |>
   ir::ir_bc_rubberband(return_bl = FALSE)
```
