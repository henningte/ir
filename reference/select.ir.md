# Subset columns in `ir` objects using column names and types

Subset columns in `ir` objects using column names and types

## Usage

``` r
select.ir(.data, ...)
```

## Source

[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

## Value

`.data` with the selected columns. If the `spectra` column is dropped,
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
[`separate.ir()`](https://henningte.github.io/ir/reference/separate.ir.md),
[`separate_rows.ir()`](https://henningte.github.io/ir/reference/separate_rows.ir.md),
[`slice`](https://henningte.github.io/ir/reference/slice.md),
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## select
dplyr::select(ir_sample_data, spectra)
#> # A tibble: 58 × 1
#>    spectra             
#>  * <named list>        
#>  1 <tibble [3,351 × 2]>
#>  2 <tibble [3,351 × 2]>
#>  3 <tibble [3,351 × 2]>
#>  4 <tibble [3,351 × 2]>
#>  5 <tibble [3,351 × 2]>
#>  6 <tibble [3,351 × 2]>
#>  7 <tibble [3,351 × 2]>
#>  8 <tibble [3,351 × 2]>
#>  9 <tibble [3,351 × 2]>
#> 10 <tibble [3,351 × 2]>
#> # ℹ 48 more rows
dplyr::select(ir_sample_data, holocellulose) # drops ir class
#> # A tibble: 58 × 1
#>    holocellulose
#>  *           [1]
#>  1         0.308
#>  2         0.250
#>  3         0.336
#>  4         0.184
#>  5         0.309
#>  6         0.335
#>  7         0.241
#>  8         0.125
#>  9         0.252
#> 10         0.349
#> # ℹ 48 more rows

```
