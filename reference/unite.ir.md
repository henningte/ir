# Unite multiple columns in an `ir` object into one by pasting strings together

Unite multiple columns in an `ir` object into one by pasting strings
together

## Usage

``` r
unite.ir(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
```

## Source

[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)

## Arguments

- data:

  An object of class `ir`.

- col:

  The name of the new column, as a string or symbol.

  This argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote strings and symbols). The name is captured from the
  expression with
  [`rlang::ensym()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  (note that this kind of interface where symbols do not represent
  actual objects is now discouraged in the tidyverse; we support it here
  for backward compatibility).

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to unite

- sep:

  Separator to use between values.

- remove:

  If `TRUE`, remove input columns from output data frame.

- na.rm:

  If `TRUE`, missing values will be removed prior to uniting each value.

## Value

`.data` with united columns. If the `spectra` column is dropped or
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
[`summarize`](https://henningte.github.io/ir/reference/summarize.md)

## Examples

``` r
## unite
ir_sample_data |>
  tidyr::separate(
    "id_sample",  c("a", "b", "c")
  ) |>
  tidyr::unite(id_sample, a, b, c)
#> # A tibble: 58 × 7
#>    id_measurement col     sample_type sample_comment klason_lignin holocellulose
#>  *          <int> <chr>   <chr>       <chr>                    [1]           [1]
#>  1              1 GN_11_… needles     Abies Firma M…         0.360         0.308
#>  2              2 GN_11_… needles     Cupressocypar…         0.339         0.250
#>  3              3 GN_11_… needles     Juniperus chi…         0.268         0.336
#>  4              4 GN_11_… needles     Metasequoia g…         0.350         0.184
#>  5              5 GN_11_… needles     Pinus strobus…         0.331         0.309
#>  6              6 GN_11_… needles     Pseudolarix a…         0.279         0.335
#>  7              7 GN_11_… needles     Sequoia sempe…         0.330         0.241
#>  8              8 GN_11_… needles     Taxodium dist…         0.357         0.125
#>  9              9 GN_11_… needles     Thuja occiden…         0.369         0.252
#> 10             10 GN_11_… needles     Tsuga carolin…         0.289         0.349
#> # ℹ 48 more rows
#> # ℹ 1 more variable: spectra <named list>

```
