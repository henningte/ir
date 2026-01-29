# Rename columns in `ir` objects

Rename columns in `ir` objects

## Usage

``` r
rename.ir(.data, ...)

rename_with.ir(.data, .fn, .cols = dplyr::everything(), ...)
```

## Source

[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  For `rename()`:
  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Use `new_name = old_name` to rename selected variables.

  For
  [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html):
  additional arguments passed onto `.fn`.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to rename; defaults to all columns.

## Value

`.data` with renamed columns. If the `spectra` column is renamed, and no
new valid `spectra` column is created, the `ir` class is dropped, else
the object is of class `ir`.

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
[`filter-joins`](https://henningte.github.io/ir/reference/filter-joins.md),
[`filter.ir()`](https://henningte.github.io/ir/reference/filter.ir.md),
[`group_by`](https://henningte.github.io/ir/reference/group_by.md),
[`mutate`](https://henningte.github.io/ir/reference/mutate.md),
[`mutate-joins`](https://henningte.github.io/ir/reference/mutate-joins.md),
[`nest`](https://henningte.github.io/ir/reference/nest.md),
[`pivot_longer.ir()`](https://henningte.github.io/ir/reference/pivot_longer.ir.md),
[`pivot_wider.ir()`](https://henningte.github.io/ir/reference/pivot_wider.ir.md),
[`rowwise.ir()`](https://henningte.github.io/ir/reference/rowwise.ir.md),
[`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md),
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## rename
dplyr::rename(ir_sample_data, hol = "holocellulose")
#> # A tibble: 58 × 7
#>    id_measurement id_sample sample_type sample_comment       klason_lignin   hol
#>  *          <int> <chr>     <chr>       <chr>                          [1]   [1]
#>  1              1 GN 11-389 needles     Abies Firma Momi fir         0.360 0.308
#>  2              2 GN 11-400 needles     Cupressocyparis ley…         0.339 0.250
#>  3              3 GN 11-407 needles     Juniperus chinensis…         0.268 0.336
#>  4              4 GN 11-411 needles     Metasequoia glyptos…         0.350 0.184
#>  5              5 GN 11-416 needles     Pinus strobus Torul…         0.331 0.309
#>  6              6 GN 11-419 needles     Pseudolarix amabili…         0.279 0.335
#>  7              7 GN 11-422 needles     Sequoia semperviren…         0.330 0.241
#>  8              8 GN 11-423 needles     Taxodium distichum …         0.357 0.125
#>  9              9 GN 11-428 needles     Thuja occidentalis …         0.369 0.252
#> 10             10 GN 11-434 needles     Tsuga caroliniana C…         0.289 0.349
#> # ℹ 48 more rows
#> # ℹ 1 more variable: spectra <named list>
dplyr::rename(ir_sample_data, spec = "spectra") # drops ir class
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
#> # ℹ 2 more variables: holocellulose [1], spec <named list>


## rename_with
dplyr::rename_with(ir_sample_data, .cols = dplyr::starts_with("id_"),
  toupper)
#> # A tibble: 58 × 7
#>    ID_MEASUREMENT ID_SAMPLE sample_type sample_comment             klason_lignin
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
dplyr::rename_with(ir_sample_data, toupper) # drops ir class
#> # A tibble: 58 × 7
#>    ID_MEASUREMENT ID_SAMPLE SAMPLE_TYPE SAMPLE_COMMENT             KLASON_LIGNIN
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
#> # ℹ 2 more variables: HOLOCELLULOSE [1], SPECTRA <named list>

```
