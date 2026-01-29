# Separate a character column in an `ir` object into multiple columns with a regular expression or numeric locations

Separate a character column in an `ir` object into multiple columns with
a regular expression or numeric locations

## Usage

``` r
separate.ir(
  data,
  col,
  into,
  sep = "[^[:alnum:]]+",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn",
  ...
)
```

## Source

[`tidyr::separate()`](https://tidyr.tidyverse.org/reference/separate.html)

## Arguments

- data:

  An object of class `ir`.

- col:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Column to expand.

- into:

  Names of new variables to create as character vector. Use `NA` to omit
  the variable in the output.

- sep:

  Separator between columns.

  If character, `sep` is interpreted as a regular expression. The
  default value is a regular expression that matches any sequence of
  non-alphanumeric values.

  If numeric, `sep` is interpreted as character positions to split at.
  Positive values start at 1 at the far-left of the string; negative
  value start at -1 at the far-right of the string. The length of `sep`
  should be one less than `into`.

- remove:

  If `TRUE`, remove input column from output data frame.

- convert:

  If `TRUE`, will run
  [`type.convert()`](https://rdrr.io/r/utils/type.convert.html) with
  `as.is = TRUE` on new columns. This is useful if the component columns
  are integer, numeric or logical.

  NB: this will cause string `"NA"`s to be converted to `NA`s.

- extra:

  If `sep` is a character vector, this controls what happens when there
  are too many pieces. There are three valid options:

  - `"warn"` (the default): emit a warning and drop extra values.

  - `"drop"`: drop any extra values without a warning.

  - `"merge"`: only splits at most `length(into)` times

- fill:

  If `sep` is a character vector, this controls what happens when there
  are not enough pieces. There are three valid options:

  - `"warn"` (the default): emit a warning and fill from the right

  - `"right"`: fill with missing values on the right

  - `"left"`: fill with missing values on the left

- ...:

  Additional arguments passed on to methods.

## Value

`.data` with separated columns. If the `spectra` column is dropped or
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
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## separate
ir_sample_data |>
  tidyr::separate(
    col = "id_sample",  c("a", "b", "c")
  )
#> # A tibble: 58 × 9
#>    id_measurement a     b     c     sample_type sample_comment     klason_lignin
#>  *          <int> <chr> <chr> <chr> <chr>       <chr>                        [1]
#>  1              1 GN    11    389   needles     Abies Firma Momi …         0.360
#>  2              2 GN    11    400   needles     Cupressocyparis l…         0.339
#>  3              3 GN    11    407   needles     Juniperus chinens…         0.268
#>  4              4 GN    11    411   needles     Metasequoia glypt…         0.350
#>  5              5 GN    11    416   needles     Pinus strobus Tor…         0.331
#>  6              6 GN    11    419   needles     Pseudolarix amabi…         0.279
#>  7              7 GN    11    422   needles     Sequoia sempervir…         0.330
#>  8              8 GN    11    423   needles     Taxodium distichu…         0.357
#>  9              9 GN    11    428   needles     Thuja occidentali…         0.369
#> 10             10 GN    11    434   needles     Tsuga caroliniana…         0.289
#> # ℹ 48 more rows
#> # ℹ 2 more variables: holocellulose [1], spectra <named list>

```
