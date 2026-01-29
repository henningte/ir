# Performs baseline correction on infrared spectra using a polynomial

`ir_bc_polynomial` performs baseline correction for infrared spectra
using a polynomial. `ir_bc_polynomial` is an extended wrapper function
for
[`ChemoSpec::baselineSpectra()`](https://bryanhanson.github.io/ChemoSpec/reference/baselineSpectra.html).

## Usage

``` r
ir_bc_polynomial(x, degree = 2, return_bl = FALSE, ...)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- degree:

  An integer value representing the degree of the polynomial used for
  baseline correction.

- return_bl:

  A logical value indicating if for each spectrum the baseline should be
  returned instead of the corrected intensity values
  (`return_bl = TRUE`) or not (`return_bl = FALSE`).

- ...:

  Ignored.

## Value

An object of class `ir` with the baseline corrected spectra if
`returnbl = FALSE` or the baselines if `returnbl = TRUE`.

## See also

[`ir_bc()`](https://henningte.github.io/ir/reference/ir_bc.md)

## Examples

``` r
if(! requireNamespace("ChemoSpec", quietly = TRUE)) {
  x2 <-
     ir::ir_sample_data |>
     ir::ir_bc_polynomial(degree = 2, return_bl = FALSE)
}
```
