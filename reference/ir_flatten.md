# Converts objects of class `ir` to objects of class `ir_flat`

`ir_flatten` takes and object of class `ir`, extracts the `spectra`
column and combines the spectra into an object of class
[`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md).
Metadata are not retained during flattening.

## Usage

``` r
ir_flatten(x, measurement_id = as.character(seq_len(nrow(x))))
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- measurement_id:

  A character vector an element for each row in `x` that contains the
  names to use as column names for the spectra in the `ir_flat` object
  to create.

## Value

An object of class
[`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md).

## Examples

``` r
x_flat <-
   ir::ir_sample_data |>
   ir::ir_flatten()
```
