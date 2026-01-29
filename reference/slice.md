# Subset rows in `ir` objects using their positions

Subset rows in `ir` objects using their positions

## Usage

``` r
slice.ir(.data, ..., .preserve = FALSE)

slice_sample.ir(.data, ..., n, prop, weight_by = NULL, replace = FALSE)
```

## Source

[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  For `slice()`:
  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Integer row values.

  Provide either positive values to keep, or negative values to drop.
  The values provided must be either all positive or all negative.
  Indices beyond the number of rows in the input are silently ignored.

  For `slice_*()`, these arguments are passed on to methods.

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used. If `n`
  is greater than the number of rows in the group (or `prop > 1`), the
  result will be silently truncated to the group size. `prop` will be
  rounded towards zero to generate an integer number of rows.

  A negative value of `n` or `prop` will be subtracted from the group
  size. For example, `n = -2` with a group of 5 rows will select 5 - 2 =
  3 rows; `prop = -0.25` with 8 rows will select 8 \* (1 - 0.25) = 6
  rows.

- weight_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Sampling weights. This must evaluate to a vector of non-negative
  numbers the same length as the input. Weights are automatically
  standardised to sum to 1.

- replace:

  Should sampling be performed with (`TRUE`) or without (`FALSE`, the
  default) replacement.

## Value

`.data` with subsetted rows.

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
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## slice
dplyr::slice(ir_sample_data, 1:5)
#> # A tibble: 5 × 7
#>   id_measurement id_sample sample_type sample_comment              klason_lignin
#> *          <int> <chr>     <chr>       <chr>                                 [1]
#> 1              1 GN 11-389 needles     Abies Firma Momi fir                0.360
#> 2              2 GN 11-400 needles     Cupressocyparis leylandii …         0.339
#> 3              3 GN 11-407 needles     Juniperus chinensis Chines…         0.268
#> 4              4 GN 11-411 needles     Metasequoia glyptostroboid…         0.350
#> 5              5 GN 11-416 needles     Pinus strobus Torulosa              0.331
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>
dplyr::slice_min(ir_sample_data, holocellulose, n = 3)
#> # A tibble: 3 × 7
#>   id_measurement id_sample sample_type        sample_comment       klason_lignin
#> *          <int> <chr>     <chr>              <chr>                          [1]
#> 1              8 GN 11-423 needles            Taxodium distichum …         0.357
#> 2             32 LG 11-418 leaves and grasses Prunus incisa Fuji …         0.326
#> 3             33 LG 11-432 leaves and grasses Acer rubrum red map…         0.239
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>
dplyr::slice_max(ir_sample_data, holocellulose, n = 3)
#> # A tibble: 3 × 7
#>   id_measurement id_sample  sample_type             sample_comment klason_lignin
#> *          <int> <chr>      <chr>                   <chr>                    [1]
#> 1             42 OCC 11-457 old corrugated cardboa… "OCC 1"                0.102
#> 2             43 OCC 11-462 old corrugated cardboa… "OCC 3"                0.142
#> 3             56 OFF 13-144 office paper            ""                     0.318
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>
dplyr::slice_head(ir_sample_data, n = 5)
#> # A tibble: 5 × 7
#>   id_measurement id_sample sample_type sample_comment              klason_lignin
#> *          <int> <chr>     <chr>       <chr>                                 [1]
#> 1              1 GN 11-389 needles     Abies Firma Momi fir                0.360
#> 2              2 GN 11-400 needles     Cupressocyparis leylandii …         0.339
#> 3              3 GN 11-407 needles     Juniperus chinensis Chines…         0.268
#> 4              4 GN 11-411 needles     Metasequoia glyptostroboid…         0.350
#> 5              5 GN 11-416 needles     Pinus strobus Torulosa              0.331
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>
dplyr::slice_tail(ir_sample_data, n = 5)
#> # A tibble: 5 × 7
#>   id_measurement id_sample  sample_type   sample_comment      klason_lignin
#> *          <int> <chr>      <chr>         <chr>                         [1]
#> 1             54 ONP 11-459 old newsprint "Los Angeles Times"        0.195 
#> 2             55 OFF 10-506 office paper  ""                         0.204 
#> 3             56 OFF 13-144 office paper  ""                         0.318 
#> 4             57 OFF 08-80  office paper  ""                         0.562 
#> 5             58 OFF 08-852 office paper  ""                         0.0542
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>

## slice_sample
set.seed(234)
dplyr::slice_sample(ir_sample_data, n = 3)
#> # A tibble: 3 × 7
#>   id_measurement id_sample sample_type        sample_comment       klason_lignin
#> *          <int> <chr>     <chr>              <chr>                          [1]
#> 1             33 LG 11-432 leaves and grasses Acer rubrum red map…         0.239
#> 2             31 LG 11-414 leaves and grasses Nyssa sylvatica Bla…         0.219
#> 3             34 LG 11-433 leaves and grasses Cornus florida Flow…         0.181
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>

```
