# Summarize each group in a `ir` object to fewer rows

Summarize each group in a `ir` object to fewer rows

## Usage

``` r
summarize.ir(.data, ..., .groups = NULL)

summarise.ir(.data, ..., .groups = NULL)
```

## Source

[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.html), or
    `sum(is.na(y))`.

  - A data frame, to add multiple columns from a single expression.

  **\[deprecated\]** Returning values with size 0 or \>1 was deprecated
  as of 1.1.0. Please use
  [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html) for
  this instead.

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - "drop_last": dropping the last level of grouping. This was the only
    supported option before version 1.0.0.

  - "drop": All levels of grouping are dropped.

  - "keep": Same grouping structure as `.data`.

  - "rowwise": Each row is its own group.

  When `.groups` is not specified, it is chosen based on the number of
  rows of the results:

  - If all the results have 1 row, you get "drop_last".

  - If the number of rows varies, you get "keep" (note that returning a
    variable number of rows was deprecated in favor of
    [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html),
    which also unconditionally drops all levels of grouping).

  In addition, a message informs you of that choice, unless the result
  is ungrouped, the option "dplyr.summarise.inform" is set to `FALSE`,
  or when `summarise()` is called from a function in a package.

## Value

`.data` with summarized columns. If the `spectra` column is dropped or
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
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## summarize

# select in each sample_type groups the first spectrum
ir_sample_data |>
  dplyr::group_by(sample_type) |>
  dplyr::summarize(spectra = list(spectra[[1]]))
#> # A tibble: 8 × 2
#>   sample_type              spectra             
#> * <chr>                    <list>              
#> 1 hardwood                 <tibble [3,351 × 2]>
#> 2 leaves and grasses       <tibble [3,351 × 2]>
#> 3 needles                  <tibble [3,351 × 2]>
#> 4 office paper             <tibble [3,351 × 2]>
#> 5 old corrugated cardboard <tibble [3,351 × 2]>
#> 6 old magazines            <tibble [3,351 × 2]>
#> 7 old newsprint            <tibble [3,351 × 2]>
#> 8 softwood                 <tibble [3,351 × 2]>

```
