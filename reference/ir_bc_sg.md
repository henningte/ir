# Performs baseline correction on infrared spectra using a Savitzky-Golay baseline

`ir_bc_sg` computes a smoothed version of spectra using
[`ir_smooth()`](https://henningte.github.io/ir/reference/ir_smooth.md)
with `method = "sg"` and uses this as baseline which is subtracted from
the spectra to perform a baseline correction (Lasch 2012) .

## Usage

``` r
ir_bc_sg(x, ..., return_bl = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- ...:

  Arguments passed to
  [`ir_smooth()`](https://henningte.github.io/ir/reference/ir_smooth.md)
  (except for `method` which is always set to `"sg"`).

- return_bl:

  A logical value indicating if for each spectrum the baseline should be
  returned instead of the corrected intensity values
  (`return_bl = TRUE`) or not (`return_bl = FALSE`).

## Value

An object of class `ir` with the baseline corrected spectra and, if
`returnbl = TRUE`, the baselines.

## References

Lasch P (2012). “Spectral Pre-Processing for Biomedical Vibrational
Spectroscopy and Microspectroscopic Imaging.” *Chemometrics and
Intelligent Laboratory Systems*, **117**, 100–114. ISSN 01697439.
[doi:10.1016/j.chemolab.2012.03.011](https://doi.org/10.1016/j.chemolab.2012.03.011)
.

## Examples

``` r
if(! requireNamespace("signal", quietly = TRUE)) {
  x <-
    ir::ir_sample_data |>
    ir::ir_bc_sg(p = 3, n = 199, ts = 1, m = 0, return_bl = FALSE)
}
```
