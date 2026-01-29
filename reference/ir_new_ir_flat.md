# Creates an object of class `ir_flat`

`ir_new_ir_flat` is the constructor function for objects of class
`ir_flat`. An object of class `ir_flat` is a `data.frame` where the
first column (`"x"`) contains unique x values of spectra (e.g.
wavenumbers) and all remaining columns represent intensity values from
spectra corresponding to the x values.

## Usage

``` r
ir_new_ir_flat(x)
```

## Arguments

- x:

  A `data.frame` with only numeric columns and only the first column
  name being "x".

## Value

An object of class `ir_flat`.

## Examples

``` r
x_flat <-
   ir::ir_sample_data |>
   ir::ir_flatten()
```
