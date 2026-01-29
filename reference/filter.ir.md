# Subset rows in `ir` objects using column values

Subset rows in `ir` objects using column values

## Usage

``` r
filter.ir(.data, ..., .preserve = FALSE)
```

## Source

[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the variables in `.data`. If multiple expressions are included, they
  are combined with the `&` operator. Only rows for which all conditions
  evaluate to `TRUE` are kept.

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

## Value

`.data` with filtered rows.

## See also

Other tidyverse:
[`arrange.ir()`](https://henningte.github.io/ir/reference/arrange.ir.md),
[`distinct.ir()`](https://henningte.github.io/ir/reference/distinct.ir.md),
[`extract.ir()`](https://henningte.github.io/ir/reference/extract.ir.md),
[`filter-joins`](https://henningte.github.io/ir/reference/filter-joins.md),
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
## filter
dplyr::filter(ir_sample_data, sample_type == "office paper")
#> # A tibble: 4 × 7
#>   id_measurement id_sample  sample_type  sample_comment klason_lignin
#> *          <int> <chr>      <chr>        <chr>          <units>      
#> 1             55 OFF 10-506 office paper ""             0.203574     
#> 2             56 OFF 13-144 office paper ""             0.318451     
#> 3             57 OFF 08-80  office paper ""             0.562450     
#> 4             58 OFF 08-852 office paper ""             0.054230     
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>

```
