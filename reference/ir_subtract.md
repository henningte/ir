# Subtract infrared spectra

`ir_subtract` takes two objects of class `ir`, `x` and `y`, and
subtracts the intensity values of spectra in matching rows from `y` from
that of `x`. Alternatively, takes an object of class `ir`, `x`, and a
numeric value, `y`, and subtracts `y` from all intensity values in `x`.

## Usage

``` r
ir_subtract(x, y)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- y:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md) or a
  numeic value. If `y` is an object of class `ir`, it must have the same
  number of rows as `x` and the same x axis values (e.g. wavenumber
  values) in each matching spectrum as in `x`.

## Value

`x` where for each spectrum the respective intensity values in `y` are
subtracted (if `y` is an object of class `ir`), or where for each
spectrum `y` has been subtracted from the intensity values.

## Examples

``` r
# subtracting two objects of class ir
x1 <-
  ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data)
x2 <-
  ir::ir_subtract(ir::ir_sample_data, ir::ir_sample_data[1, ])

# subtracting a numeric value from an object of class `ir`.
x3 <-
  ir::ir_subtract(ir::ir_sample_data, 20)

# subtracting a numeric vector from an object of class `ir`.
x4 <-
  ir::ir_subtract(
     ir::ir_sample_data,
     seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
  )
```
