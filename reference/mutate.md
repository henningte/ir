# Mutate an `ir` object by adding new or replacing existing columns

Mutate an `ir` object by adding new or replacing existing columns

## Usage

``` r
mutate.ir(
  .data,
  ...,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)

transmute.ir(.data, ...)
```

## Source

[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

- .keep:

  Control which columns from `.data` are retained in the output.
  Grouping columns and columns created by `...` are always kept.

  - `"all"` retains all columns from `.data`. This is the default.

  - `"used"` retains only the columns used in `...` to create new
    columns. This is useful for checking your work, as it displays
    inputs and outputs side-by-side.

  - `"unused"` retains only the columns *not* used in `...` to create
    new columns. This is useful if you generate new columns, but no
    longer need the columns used to generate them.

  - `"none"` doesn't retain any extra columns from `.data`. Only the
    grouping variables and columns created by `...` are kept.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, control where new columns should appear (the default is to
  add to the right hand side). See
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  for more details.

## Value

`.data` with modified columns. If the `spectra` column is dropped or
invalidated (see
[`ir_new_ir()`](https://henningte.github.io/ir/reference/ir_new_ir.md)),
the `ir` class is dropped, else the object is of class `ir`.

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
[`filter-joins`](https://henningte.github.io/ir/reference/filter-joins.md),
[`filter.ir()`](https://henningte.github.io/ir/reference/filter.ir.md),
[`group_by`](https://henningte.github.io/ir/reference/group_by.md),
[`mutate-joins`](https://henningte.github.io/ir/reference/mutate-joins.md),
[`nest`](https://henningte.github.io/ir/reference/nest.md),
[`pivot_longer.ir()`](https://henningte.github.io/ir/reference/pivot_longer.ir.md),
[`pivot_wider.ir()`](https://henningte.github.io/ir/reference/pivot_wider.ir.md),
[`rename`](https://henningte.github.io/ir/reference/rename.md),
[`rowwise.ir()`](https://henningte.github.io/ir/reference/rowwise.ir.md),
[`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md),
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## mutate
dplyr::mutate(ir_sample_data, hkl = klason_lignin + holocellulose)
#> # A tibble: 58 × 8
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
#> # ℹ 3 more variables: holocellulose [1], spectra <named list>, hkl [1]


## transmute
dplyr::transmute(ir_sample_data, hkl = klason_lignin + holocellulose)
#> # A tibble: 58 × 1
#>      hkl
#>  *   [1]
#>  1 0.668
#>  2 0.589
#>  3 0.604
#>  4 0.534
#>  5 0.640
#>  6 0.615
#>  7 0.570
#>  8 0.482
#>  9 0.621
#> 10 0.638
#> # ℹ 48 more rows

```
