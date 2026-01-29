# Cleans objects of class `ir_flat`

`ir_flatten_clean` takes an object of class `ir_flat` and either returns
all non-empty spectra or all empty spectra as object of class `ir_flat`.

## Usage

``` r
ir_flat_clean(x, return_empty = FALSE)
```

## Arguments

- x:

  An object of class
  [`ir_flat`](https://henningte.github.io/ir/reference/ir_new_ir_flat.md).

- return_empty:

  A logical value indicating if the empty spectra should be returned
  (`return_empty = TRUE`) or the non-empty spectra
  (`return_empty = FALSE`).

## Value

`x` where empty spectra are dropped (if `return_empty = TRUE`) or only
empty spectra are returned (`return_empty = FALSE`).
