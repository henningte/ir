# Multiply infrared spectra or multiply infrared spectra with a numeric value

`ir_multiply` takes two objects of class `ir`, `x` and `y`, and
multiplies their intensity values, or it takes one object of class `ir`,
`x`, and one numeric value, `y`, and multiplies all intensity values in
`x` with `y`.

## Usage

``` r
ir_multiply(x, y)
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

`x` where for each spectrum intensity values are multiplied with the
respective intensity values in `y` (if `y` is an object of class `ir`),
or where all intensity values are multiplied with `y` if `y` is a
numeric value.

## Examples

``` r
# multiplication with y as ir object
x1 <-
  ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data)
x2 <-
  ir::ir_multiply(ir::ir_sample_data, ir::ir_sample_data[1, ])

# multiplication with y being a numeric value
x3 <-
  ir::ir_multiply(ir::ir_sample_data, y = -1)

# multiplication with y being a numeric vector
x4 <-
  ir::ir_multiply(
     ir::ir_sample_data,
     seq(from = 0, to = 2, length.out = nrow(ir::ir_sample_data))
  )
```
