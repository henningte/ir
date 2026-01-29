# Drops the column `spectra` from an object is of class `ir`

`ir_drop_spectra` removes the column `spectra` from an object of class
`ir` and removes the `"ir"` class attribute.

## Usage

``` r
ir_drop_spectra(x)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

## Value

`x` without column `spectra` and without `"ir"` class attribute.

## Examples

``` r
ir::ir_sample_data |>
  ir_drop_spectra()
#> # A tibble: 58 × 6
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>  *          <int> <chr>     <chr>       <chr>                      <units>      
#>  1              1 GN 11-389 needles     Abies Firma Momi fir       0.359944     
#>  2              2 GN 11-400 needles     Cupressocyparis leylandii… 0.339405     
#>  3              3 GN 11-407 needles     Juniperus chinensis Chine… 0.267552     
#>  4              4 GN 11-411 needles     Metasequoia glyptostroboi… 0.350016     
#>  5              5 GN 11-416 needles     Pinus strobus Torulosa     0.331100     
#>  6              6 GN 11-419 needles     Pseudolarix amabili Golde… 0.279360     
#>  7              7 GN 11-422 needles     Sequoia sempervirens Cali… 0.329672     
#>  8              8 GN 11-423 needles     Taxodium distichum Cascad… 0.356950     
#>  9              9 GN 11-428 needles     Thuja occidentalis Easter… 0.369360     
#> 10             10 GN 11-434 needles     Tsuga caroliniana Carolin… 0.289050     
#> # ℹ 48 more rows
#> # ℹ 1 more variable: holocellulose <units>
```
