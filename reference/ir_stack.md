# Stacks a matrix or data frame with spectra into a list column

`ir_stack` takes a matrix or data frame with infrared spectra and
converts it into a list column corresponding to the column `spectra` in
objects of class `ir`.

## Usage

``` r
ir_stack(x)
```

## Arguments

- x:

  A matrix or data frame with a first column (`x`) containing "x axis
  values" of the spectra (e.g. wavenumbers) and all remaining columns
  containing intensity values of spectra.

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with the stacked spectra in column `spectra`.

## Examples

``` r
# from data frame
x1 <-
   ir::ir_sample_data |>
   ir::ir_flatten() |>
   ir::ir_stack()

# from matrix
x2 <-
   ir::ir_sample_data |>
   ir::ir_flatten() |>
   as.matrix() |>
   ir::ir_stack()
```
