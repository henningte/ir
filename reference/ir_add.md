# Add infrared spectra

`ir_add` takes two objects of class `ir`, `x` and `y`, and adds the
intensity values of spectra in matching rows from `y` to that of `x`.

## Usage

``` r
ir_add(x, y)
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
added.

## Examples

``` r
x1 <-
  ir::ir_add(ir::ir_sample_data, ir::ir_sample_data)
x2 <-
  ir::ir_add(ir::ir_sample_data, ir::ir_sample_data[1, ])

# adding a numeric value to an object of class `ir`.
x3 <-
  ir::ir_add(ir::ir_sample_data, 1)

# adding a numeric vector from an object of class `ir`.
x4 <-
  ir::ir_add(
     ir::ir_sample_data,
     seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
  )
```
