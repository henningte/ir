# Gets the index of a defined wavenumber value for a spectrum

`ir_get_wavenumberindex` gets for a defined wavenumber value or set of
wavenumber values the corresponding indices (row number) in an object of
class `ir` that has been flattened with
[`ir_flatten()`](https://henningte.github.io/ir/reference/ir_flatten.md).
If the specified wavenumber values do not match exactly the wavenumber
values in the `ir` object, the indices for the next wavenumber values
will be returned, along with a warning.

## Usage

``` r
ir_get_wavenumberindex(x, wavenumber, warn = TRUE)
```

## Arguments

- x:

  A data.frame with a column x representing the x units of a spectrum or
  several spectra (e.g. in the form of an object of class
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md)).

- wavenumber:

  A numeric vector with wavenumber values for which to get indices.

- warn:

  logical value indicating if warnings should be displayed (`TRUE`) or
  not (`FALSE`).

## Value

An integer vector with the same length as `wavenumber` with the row
indices of `x` corresponding to the wavenumber values in `wavenumber`.

## Examples

``` r
x_index_1090 <-
   ir::ir_sample_data |>
   ir::ir_flatten() |>
   ir::ir_get_wavenumberindex(wavenumber = 1090)
```
