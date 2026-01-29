# Filtering joins for an `ir` object

Filtering joins for an `ir` object

## Usage

``` r
semi_join.ir(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))

anti_join.ir(x, y, by = NULL, copy = FALSE, ..., na_matches = c("na", "never"))
```

## Source

[`filter-joins`](https://dplyr.tidyverse.org/reference/filter-joins.html)

## Arguments

- x:

  An object of class `ir`.

- y:

  A data frame.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- ...:

  Other parameters passed onto methods.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`, [`match()`](https://rdrr.io/r/base/match.html), and
    [`merge()`](https://rdrr.io/r/base/merge.html).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

## Value

`x` and `y` joined. If the `spectra` column is renamed, the `ir` class
is dropped. See
[`filter-joins`](https://dplyr.tidyverse.org/reference/filter-joins.html).

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
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
## semi_join
set.seed(234)
dplyr::semi_join(
  ir_sample_data,
  tibble::tibble(
    id_measurement = c(1:5, 101:105),
    nitrogen_content = rbeta(n = 10, 0.2, 0.1)
  ),
  by = "id_measurement"
)
#> # A tibble: 5 × 7
#>   id_measurement id_sample sample_type sample_comment              klason_lignin
#> *          <int> <chr>     <chr>       <chr>                       <units>      
#> 1              1 GN 11-389 needles     Abies Firma Momi fir        0.359944     
#> 2              2 GN 11-400 needles     Cupressocyparis leylandii … 0.339405     
#> 3              3 GN 11-407 needles     Juniperus chinensis Chines… 0.267552     
#> 4              4 GN 11-411 needles     Metasequoia glyptostroboid… 0.350016     
#> 5              5 GN 11-416 needles     Pinus strobus Torulosa      0.331100     
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>


## anti_join
set.seed(234)
dplyr::anti_join(
  ir_sample_data,
  tibble::tibble(
    id_measurement = c(1:5, 101:105),
    nitrogen_content = rbeta(n = 10, 0.2, 0.1)
  ),
  by = "id_measurement"
)
#> # A tibble: 53 × 7
#>    id_measurement id_sample sample_type sample_comment             klason_lignin
#>  *          <int> <chr>     <chr>       <chr>                      <units>      
#>  1              6 GN 11-419 needles     Pseudolarix amabili Golde… 0.279360     
#>  2              7 GN 11-422 needles     Sequoia sempervirens Cali… 0.329672     
#>  3              8 GN 11-423 needles     Taxodium distichum Cascad… 0.356950     
#>  4              9 GN 11-428 needles     Thuja occidentalis Easter… 0.369360     
#>  5             10 GN 11-434 needles     Tsuga caroliniana Carolin… 0.289050     
#>  6             11 GN 11-435 needles     Picea abies Norway Spruce  0.288000     
#>  7             12 GN 11-460 needles     Pinus taeda Loblolly pine  0.322300     
#>  8             13 HW 07-151 hardwood    Quercus sp. Red oak (from… 0.238095     
#>  9             14 HW 11-137 hardwood    Acer saccharum Sugar maple 0.242592     
#> 10             15 HW 11-144 hardwood    Fraxinus americana White … 0.259224     
#> # ℹ 43 more rows
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>

```
