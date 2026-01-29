# Group input `ir` objects by rows

Group input `ir` objects by rows

## Usage

``` r
rowwise.ir(.data, ...)
```

## Source

[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)

## Arguments

- .data:

  Input data frame.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Variables to be preserved when calling
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).
  This is typically a set of variables whose combination uniquely
  identify each row.

  **NB**: unlike
  [`group_by()`](https://henningte.github.io/ir/reference/group_by.md)
  you can not create new variables here but instead you can select
  multiple variables with (e.g.)
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

- data:

  An object of class `ir`.

## Value

`data` as row-wise data frame. See
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html).

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
[`rename`](https://henningte.github.io/ir/reference/rename.md),
[`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md),
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## rowwise
dplyr::rowwise(ir_sample_data) |>
  dplyr::mutate(
    hkl =
      mean(
        units::drop_units(klason_lignin),
        units::drop_units(holocellulose)
      )
  )
#> # A tibble: 58 × 8
#> # Rowwise: 
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
#> # ℹ 3 more variables: holocellulose [1], spectra <named list>, hkl <dbl>

```
