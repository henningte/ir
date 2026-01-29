# Replicate ir objects

`rep.ir` is the replicate method for
[`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md) objects.
Replicating and `ir` object means to replicate its rows and bind these
together to a larger `ir` object.

## Usage

``` r
# S3 method for class 'ir'
rep(x, ...)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- ...:

  See [`rep()`](https://rdrr.io/r/base/rep.html).

## Value

An object of class `ir` with replicated spectra.

## Examples

``` r
# replicate the sample data
x <- rep(ir::ir_sample_data, times = 2)
x <- rep(ir::ir_sample_data, each = 2)
x <- rep(ir::ir_sample_data, length.out = 3)
```
