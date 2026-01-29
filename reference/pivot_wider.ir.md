# Pivot an `ir` object from wide to long

Pivot an `ir` object from wide to long

## Usage

``` r
pivot_wider.ir(
  data,
  id_cols = NULL,
  names_from = "name",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_repair = "check_unique",
  values_from = "value",
  values_fill = NULL,
  values_fn = NULL,
  ...
)
```

## Source

[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)

## Arguments

- data:

  An object of class `ir`.

- id_cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A set of columns that uniquely identify each observation. Typically
  used when you have redundant variables, i.e. variables whose values
  are perfectly correlated with existing variables.

  Defaults to all columns in `data` except for the columns specified
  through `names_from` and `values_from`. If a tidyselect expression is
  supplied, it will be evaluated on `data` after removing the columns
  specified through `names_from` and `values_from`.

- names_from, values_from:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A pair of arguments describing which column (or columns) to get the
  name of the output column (`names_from`), and which column (or
  columns) to get the cell values from (`values_from`).

  If `values_from` contains multiple values, the value will be added to
  the front of the output column.

- names_prefix:

  String added to the start of every variable name. This is particularly
  useful if `names_from` is a numeric vector and you want to create
  syntactic variable names.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_glue:

  Instead of `names_sep` and `names_prefix`, you can supply a glue
  specification that uses the `names_from` columns (and special
  `.value`) to create custom column names.

- names_sort:

  Should the column names be sorted? If `FALSE`, the default, column
  names are ordered by first appearance.

- names_repair:

  What happens if the output has invalid column names? The default,
  `"check_unique"` is to error if the columns are duplicated. Use
  `"minimal"` to allow duplicates in the output, or `"unique"` to
  de-duplicated by adding numeric suffixes. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more options.

- values_fill:

  Optionally, a (scalar) value that specifies what each `value` should
  be filled in with when missing.

  This can be a named list if you want to apply different fill values to
  different value columns.

- values_fn:

  Optionally, a function applied to the value in each cell in the
  output. You will typically use this when the combination of `id_cols`
  and `names_from` columns does not uniquely identify an observation.

  This can be a named list if you want to apply different aggregations
  to different `values_from` columns.

- ...:

  Additional arguments passed on to methods.

## Value

`data` in a wide format. If the `spectra` column is dropped or
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
## pivot_wider
ir_sample_data |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(c("holocellulose", "klason_lignin"))
  ) |>
  tidyr::pivot_wider(names_from = "name", values_from = "value")
#> # A tibble: 58 × 7
#>    id_measurement id_sample sample_type sample_comment    spectra  holocellulose
#>             <int> <chr>     <chr>       <chr>             <named >           [1]
#>  1              1 GN 11-389 needles     Abies Firma Momi… <tibble>         0.308
#>  2              2 GN 11-400 needles     Cupressocyparis … <tibble>         0.250
#>  3              3 GN 11-407 needles     Juniperus chinen… <tibble>         0.336
#>  4              4 GN 11-411 needles     Metasequoia glyp… <tibble>         0.184
#>  5              5 GN 11-416 needles     Pinus strobus To… <tibble>         0.309
#>  6              6 GN 11-419 needles     Pseudolarix amab… <tibble>         0.335
#>  7              7 GN 11-422 needles     Sequoia sempervi… <tibble>         0.241
#>  8              8 GN 11-423 needles     Taxodium distich… <tibble>         0.125
#>  9              9 GN 11-428 needles     Thuja occidental… <tibble>         0.252
#> 10             10 GN 11-434 needles     Tsuga carolinian… <tibble>         0.349
#> # ℹ 48 more rows
#> # ℹ 1 more variable: klason_lignin [1]

```
