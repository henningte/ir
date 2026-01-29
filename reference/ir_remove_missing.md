# Removes empty data values in an object of class `ir`

`ir_remove_missing` takes and object of class `ir` and removes all rows
in the `data.frame`s of the list column `spectra` that have `NA`
intensity values (column `y`). Additionally, one can specify to remove
rows in the `ir` object to discard if they contain empty spectra.

## Usage

``` r
ir_remove_missing(x, remove_rows = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- remove_rows:

  A logical value indicating if rows in `x` with empty spectra should be
  discarded (`remove_rows = TRUE`) or not (`remove_rows = FALSE`).

## Value

`x` with cleaned spectra.

## Examples

``` r
# create sample data with some missing rows and one entire missing spectra
x <-
   ir::ir_sample_data
x$spectra[[1]] <- x$spectra[[1]][0, ]
x$spectra[[2]][1:100, "y"] <- NA_real_

# remove missing values (but remove no rows in x)
x1 <-
   x |>
   ir::ir_remove_missing(remove_rows = FALSE)

# remove missing values (and remove rows in x if a compete spectrum is
# missing)
x2 <-
   x |>
   ir::ir_remove_missing(remove_rows = TRUE)

nrow(x)
#> [1] 58
nrow(x1)
#> [1] 58
nrow(x2)
#> [1] 57
```
