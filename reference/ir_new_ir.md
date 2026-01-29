# Creates an object of class `ir`

`ir_new_ir` is the constructor function for objects of class `ir`. An
object of class `ir` is a
[`tibble::tbl_df()`](https://tibble.tidyverse.org/reference/tbl_df-class.html)
with a sample in each row and a list column containing spectra for each
sample.

## Usage

``` r
ir_new_ir(spectra, metadata = tibble::tibble())
```

## Arguments

- spectra:

  A named list in which each element contains spectral data for one
  measurement. Each list element must be a `data.frame` with two columns
  and a row for each wavenumber value in the spectra data. The first
  column must contain unique wavenumber values and the second column
  intensity values of the measured spectrum of the sample.

- metadata:

  An optional `data.frame` with additional columns containing metadata
  for the spectra in `spectra`. Optionally, an empty `data.frame` can be
  defined if no metadata are available.

## Value

An object of class `ir` with the following columns:

- spectra:

  A list column identical to `spectra`.

- ...:

  Additional columns contained in `metadata`.

## Examples

``` r
ir_new_ir(
  spectra = ir_sample_data$spectra,
  metadata = ir_sample_data |> dplyr::select(-spectra)
)
#> # A tibble: 58 × 7
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>  *          <int> <chr>     <chr>       <chr>                                [1]
#>  1              1 GN 11-389 needles     Abies Firma Momi fir               0.360
#>  2              2 GN 11-400 needles     Cupressocyparis leylandii…         0.339
#>  3              3 GN 11-407 needles     Juniperus chinensis Chine…         0.268
#>  4              4 GN 11-411 needles     Metasequoia glyptostroboi…         0.350
#>  5              5 GN 11-416 needles     Pinus strobus Torulosa             0.331
#>  6              6 GN 11-419 needles     Pseudolarix amabili Golde…         0.279
#>  7              7 GN 11-422 needles     Sequoia sempervirens Cali…         0.330
#>  8              8 GN 11-423 needles     Taxodium distichum Cascad…         0.357
#>  9              9 GN 11-428 needles     Thuja occidentalis Easter…         0.369
#> 10             10 GN 11-434 needles     Tsuga caroliniana Carolin…         0.289
#> # ℹ 48 more rows
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>
```
