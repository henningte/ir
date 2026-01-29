# Group rows in `ir` objects by one or more variables

Group rows in `ir` objects by one or more variables

## Usage

``` r
group_by.ir(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
)

ungroup.ir(.data, ...)
```

## Source

[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  In `group_by()`, variables or computations to group by. Computations
  are always done on the ungrouped data frame. To perform computations
  on the grouped data, you need to use a separate
  [`mutate()`](https://henningte.github.io/ir/reference/mutate.md) step
  before the `group_by()`. Computations are not allowed in `nest_by()`.
  In `ungroup()`, variables to remove from the grouping.

- .add:

  When `FALSE`, the default, `group_by()` will override existing groups.
  To add to the existing groups, use `.add = TRUE`.

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

## Value

`.data` with grouped rows (`group_by.ir()`) or ungrouped rows
(`ungroup.ir()`).

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
[`filter-joins`](https://henningte.github.io/ir/reference/filter-joins.md),
[`filter.ir()`](https://henningte.github.io/ir/reference/filter.ir.md),
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
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## group_by
dplyr::group_by(ir_sample_data, sample_type)
#> # A tibble: 58 × 7
#> # Groups:   sample_type [8]
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>


## ungroup
dplyr::ungroup(dplyr::group_by(ir_sample_data, sample_type))
#> # A tibble: 58 × 7
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
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>

```
