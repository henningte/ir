# Plots an object of class `ir`

`plot.ir` is the plot method for objects of class `ir`.

## Usage

``` r
# S3 method for class 'ir'
plot(x, ...)
```

## Arguments

- x:

  An object of class
  [`ir`](https://henningte.github.io/ir/reference/ir_new_ir.md).

- ...:

  Further arguments, will be ignored.

## Value

An object of class
[`ggplot2`](https://ggplot2.tidyverse.org/reference/ggplot.html).

## Examples

``` r
# simple plotting
plot(ir::ir_sample_data[1:2, ])


# advanced functions
plot(ir::ir_sample_data) +
   ggplot2::facet_wrap(~ sample_type)

```
