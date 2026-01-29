# Averages infrared spectra within groups

`ir_average` averages infrared spectra within a user-defined group. `NA`
values are omitted by default.

## Usage

``` r
ir_average(x, ..., na.rm = TRUE, .groups = "drop")
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- ...:

  Variables in `x` to use as groups.

- na.rm:

  A logical value indicating if `NA` values should be dropped (`TRUE`)
  or not (`FALSE`).

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
  or when
  [`summarise()`](https://henningte.github.io/ir/reference/summarize.md)
  is called from a function in a package.

## Value

An object of class `ir` where spectra have been averaged within groups
defined by `...`.

## Examples

``` r
# average the sample data spectra across sample types
x <-
  ir_sample_data |>
  ir_average(sample_type)
```
