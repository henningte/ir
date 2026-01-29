# Extract a character column in an `ir` object into multiple columns using regular expression groups

Extract a character column in an `ir` object into multiple columns using
regular expression groups

## Usage

``` r
extract.ir(
  data,
  col,
  into,
  regex = "([[:alnum:]]+)",
  remove = TRUE,
  convert = FALSE,
  ...
)
```

## Source

[`tidyr::extract()`](https://tidyr.tidyverse.org/reference/extract.html)

## Arguments

- data:

  An object of class `ir`.

- col:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Column to expand.

- into:

  Names of new variables to create as character vector. Use `NA` to omit
  the variable in the output.

- regex:

  A string representing a regular expression used to extract the desired
  values. There should be one group (defined by `()`) for each element
  of `into`.

- remove:

  If `TRUE`, remove input column from output data frame.

- convert:

  If `TRUE`, will run
  [`type.convert()`](https://rdrr.io/r/utils/type.convert.html) with
  `as.is = TRUE` on new columns. This is useful if the component columns
  are integer, numeric or logical.

  NB: this will cause string `"NA"`s to be converted to `NA`s.

- ...:

  Additional arguments passed on to methods.

## Value

`data` with an extracted character column. See
[`tidyr::extract()`](https://tidyr.tidyverse.org/reference/extract.html).

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
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
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## extract
ir_sample_data |>
  tidyr::extract(
    id_sample,  "a"
  )
#> # A tibble: 58 × 7
#>    id_measurement a     sample_type sample_comment   klason_lignin holocellulose
#>             <int> <chr> <chr>       <chr>            <units>       <units>      
#>  1              1 GN    needles     Abies Firma Mom… 0.359944      0.3084       
#>  2              2 GN    needles     Cupressocyparis… 0.339405      0.2497       
#>  3              3 GN    needles     Juniperus chine… 0.267552      0.3362       
#>  4              4 GN    needles     Metasequoia gly… 0.350016      0.1844       
#>  5              5 GN    needles     Pinus strobus T… 0.331100      0.3092       
#>  6              6 GN    needles     Pseudolarix ama… 0.279360      0.3355       
#>  7              7 GN    needles     Sequoia semperv… 0.329672      0.2408       
#>  8              8 GN    needles     Taxodium distic… 0.356950      0.1253       
#>  9              9 GN    needles     Thuja occidenta… 0.369360      0.2517       
#> 10             10 GN    needles     Tsuga carolinia… 0.289050      0.3487       
#> # ℹ 48 more rows
#> # ℹ 1 more variable: spectra <named list>

```
