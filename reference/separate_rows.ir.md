# Separate a collapsed column in an `ir` object into multiple rows

Separate a collapsed column in an `ir` object into multiple rows

## Usage

``` r
separate_rows.ir(data, ..., sep = "[^[:alnum:].]+", convert = FALSE)
```

## Source

[`tidyr::separate_rows()`](https://tidyr.tidyverse.org/reference/separate_rows.html)

## Arguments

- data:

  An object of class `ir`.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to separate across multiple rows

- sep:

  Separator delimiting collapsed values.

- convert:

  If `TRUE` will automatically run
  [`type.convert()`](https://rdrr.io/r/utils/type.convert.html) on the
  key column. This is useful if the column types are actually numeric,
  integer, or logical.

## Value

`data` with a collapsed column separated into multiple rows. See
[`tidyr::separate_rows()`](https://tidyr.tidyverse.org/reference/separate_rows.html).

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
[`rowwise.ir()`](https://henningte.github.io/ir/reference/rowwise.ir.md),
[`select.ir()`](https://henningte.github.io/ir/reference/select.ir.md),
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## separate_rows
ir_sample_data |>
  tidyr::unite(
    col = content, holocellulose, klason_lignin
  ) |>
  tidyr::separate_rows(
    col
  )
#> # A tibble: 116 × 6
#>    id_measurement id_sample sample_type sample_comment            col   spectra 
#>  *          <int> <chr>     <chr>       <chr>                     <chr> <named >
#>  1              1 GN 11-389 needles     Abies Firma Momi fir      0.30… <tibble>
#>  2              1 GN 11-389 needles     Abies Firma Momi fir      0.35… <tibble>
#>  3              2 GN 11-400 needles     Cupressocyparis leylandi… 0.24… <tibble>
#>  4              2 GN 11-400 needles     Cupressocyparis leylandi… 0.33… <tibble>
#>  5              3 GN 11-407 needles     Juniperus chinensis Chin… 0.33… <tibble>
#>  6              3 GN 11-407 needles     Juniperus chinensis Chin… 0.26… <tibble>
#>  7              4 GN 11-411 needles     Metasequoia glyptostrobo… 0.18… <tibble>
#>  8              4 GN 11-411 needles     Metasequoia glyptostrobo… 0.35… <tibble>
#>  9              5 GN 11-416 needles     Pinus strobus Torulosa    0.30… <tibble>
#> 10              5 GN 11-416 needles     Pinus strobus Torulosa    0.33… <tibble>
#> # ℹ 106 more rows

```
