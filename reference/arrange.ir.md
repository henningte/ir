# Arrange rows in `ir` objects by column values

Arrange rows in `ir` objects by column values

## Usage

``` r
arrange.ir(.data, ..., .by_group = FALSE)
```

## Source

[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

## Arguments

- .data:

  An object of class `ir`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  data frames only.

## Value

`.data` with arranged rows.

## See also

Other tidyverse:
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
[`summarize`](https://henningte.github.io/ir/reference/summarize.md),
[`unite.ir()`](https://henningte.github.io/ir/reference/unite.ir.md)

## Examples

``` r
## arrange
dplyr::arrange(ir_sample_data, dplyr::desc(sample_type))
#> # A tibble: 58 × 7
#>    id_measurement id_sample  sample_type   sample_comment          klason_lignin
#>  *          <int> <chr>      <chr>         <chr>                   <units>      
#>  1             37 SW 11-138  softwood      Tsuga heterophyla West… 0.347116     
#>  2             38 SW 11-139  softwood      Pinus monticola Wester… 0.274905     
#>  3             39 SW 11-154  softwood      Pinus strobus Eastern … 0.311760     
#>  4             40 SW 11-158  softwood      Pinus taeda Loblolly p… 0.274715     
#>  5             41 SW 11-178  softwood      Tsuga caroliniana Caro… 0.345870     
#>  6             49 ONP 08-78  old newsprint Australia Paper         0.240786     
#>  7             50 ONP 09-388 old newsprint Eleazer 1997 sample     0.238994     
#>  8             51 ONP 11-450 old newsprint News and Observer date… 0.163485     
#>  9             52 ONP 11-451 old newsprint Houston Chronicle date… 0.240492     
#> 10             53 ONP 11-458 old newsprint Indianapolis star       0.288420     
#> # ℹ 48 more rows
#> # ℹ 2 more variables: holocellulose <units>, spectra <named list>

```
