# Nest and un-nest an `ir` object

Nest and un-nest an `ir` object

## Usage

``` r
nest.ir(.data, ..., .names_sep = NULL, .key = deprecated())

unnest.ir(
  data,
  cols,
  ...,
  keep_empty = FALSE,
  ptype = NULL,
  names_sep = NULL,
  names_repair = "check_unique",
  .drop = deprecated(),
  .id = deprecated(),
  .sep = deprecated(),
  .preserve = deprecated()
)
```

## Source

[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to nest; these will appear in the inner data frames.

  Specified using name-variable pairs of the form
  `new_col = c(col1, col2, col3)`. The right hand side can be any valid
  tidyselect expression.

  If not supplied, then `...` is derived as all columns *not* selected
  by `.by`, and will use the column name from `.key`.

  **\[deprecated\]**: previously you could write `df |> nest(x, y, z)`.
  Convert to `df |> nest(data = c(x, y, z))`.

- .key:

  The name of the resulting nested column. Only applicable when `...`
  isn't specified, i.e. in the case of `df |> nest(.by = x)`.

  If `NULL`, then `"data"` will be used by default.

- data:

  A data frame.

- cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  List-columns to unnest.

  When selecting multiple columns, values from the same row will be
  recycled to their common size.

- keep_empty:

  By default, you get one row of output for each element of the list
  that you are unchopping/unnesting. This means that if there's a size-0
  element (like `NULL` or an empty data frame or vector), then that
  entire row will be dropped from the output. If you want to preserve
  all rows, use `keep_empty = TRUE` to replace size-0 elements with a
  single row of missing values.

- ptype:

  Optionally, a named list of column name-prototype pairs to coerce
  `cols` to, overriding the default that will be guessed from combining
  the individual values. Alternatively, a single empty ptype can be
  supplied, which will be applied to all `cols`.

- names_sep, .names_sep:

  If `NULL`, the default, the names will be left as is. In `nest()`,
  inner names will come from the former outer names; in `unnest()`, the
  new outer names will come from the inner names.

  If a string, the inner and outer names will be used together. In
  `unnest()`, the names of the new outer columns will be formed by
  pasting together the outer and the inner column names, separated by
  `names_sep`. In `nest()`, the new inner names will have the outer
  names + `names_sep` automatically stripped. This makes `names_sep`
  roughly symmetric between nesting and unnesting.

- names_repair:

  Used to check that output data frame has valid names. Must be one of
  the following options:

  - `"minimal`": no name repair or checks, beyond basic existence,

  - `"unique`": make sure names are unique and not empty,

  - `"check_unique`": (the default), no name repair, but check they are
    unique,

  - `"universal`": make the names unique and syntactic

  - a function: apply custom name repair.

  - [tidyr_legacy](https://tidyr.tidyverse.org/reference/tidyr_legacy.html):
    use the name repair from tidyr 0.8.

  - a formula: a purrr-style anonymous function (see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html))

  See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more details on these terms and the strategies used to enforce
  them.

- .drop, .preserve:

  **\[deprecated\]**: all list-columns are now preserved; If there are
  any that you don't want in the output use
  [`select()`](https://dplyr.tidyverse.org/reference/select.html) to
  remove them prior to unnesting.

- .id:

  **\[deprecated\]**: convert `df |> unnest(x, .id = "id")` to
  `df |> mutate(id = names(x)) |> unnest(x))`.

- .sep:

  **\[deprecated\]**: use `names_sep` instead.

## Value

`.data` with nested or unnested columns. If the `spectra` column is
dropped or invalidated (see
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
## nest
ir_sample_data |>
  tidyr::nest(
    contents = c(holocellulose, klason_lignin)
  )
#> # A tibble: 58 × 6
#>    id_measurement id_sample sample_type sample_comment         spectra  contents
#>  *          <int> <chr>     <chr>       <chr>                  <named > <list>  
#>  1              1 GN 11-389 needles     Abies Firma Momi fir   <tibble> <tibble>
#>  2              2 GN 11-400 needles     Cupressocyparis leyla… <tibble> <tibble>
#>  3              3 GN 11-407 needles     Juniperus chinensis C… <tibble> <tibble>
#>  4              4 GN 11-411 needles     Metasequoia glyptostr… <tibble> <tibble>
#>  5              5 GN 11-416 needles     Pinus strobus Torulosa <tibble> <tibble>
#>  6              6 GN 11-419 needles     Pseudolarix amabili G… <tibble> <tibble>
#>  7              7 GN 11-422 needles     Sequoia sempervirens … <tibble> <tibble>
#>  8              8 GN 11-423 needles     Taxodium distichum Ca… <tibble> <tibble>
#>  9              9 GN 11-428 needles     Thuja occidentalis Ea… <tibble> <tibble>
#> 10             10 GN 11-434 needles     Tsuga caroliniana Car… <tibble> <tibble>
#> # ℹ 48 more rows


## unnest
ir_sample_data |>
  tidyr::nest(
    contents = c(holocellulose, klason_lignin)
  ) |>
  tidyr::unnest("contents")
#> # A tibble: 58 × 7
#>    id_measurement id_sample sample_type sample_comment    spectra  holocellulose
#>  *          <int> <chr>     <chr>       <chr>             <named >           [1]
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
